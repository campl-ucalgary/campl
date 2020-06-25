module ToCMPL.LamLift_Help where 

import TypeInfer.MPL_AST
import Control.Monad.State
import Data.Maybe

{-
This function will generate fresh function names and arguments.
-}

freshFNames_Args :: [Defn] -> Term ->
                    State (Int,Int) (Term,[Defn])
freshFNames_Args [] term
    = return (term,[])
freshFNames_Args dlist term = do 
    let 
      flist = map getFunNames dlist 
    substList <- freshFunNames flist 
    let 
      newDefns = map (freshFNames_Term substList) dlist
      newTerm  = renameFCall substList term
    return (newTerm,newDefns) 


{-
This function replaces the old function names with their 
newly coined names.
-}

freshFNames_Term :: [(Name,Name)] ->  Defn -> Defn
freshFNames_Term substList (FunctionDefn (Custom fnm,fT,pattTerms,pn)) 
    = FunctionDefn (Custom newName,fT,newPattTerms,pn)
  where 
      newName  
          = (fromJust.lookup fnm) substList
      newPattTerms
          = map (freshFNames_PattTerm substList) pattTerms

freshFNames_PattTerm :: [(Name,Name)] -> (PatternTermPhr,PosnPair) -> 
                        (PatternTermPhr,PosnPair)

freshFNames_PattTerm slist ((patts,Left term),pn)    
      = ((patts,Left newTerm),pn)
  where 
    newTerm = renameFCall slist term

getFunNames :: Defn -> Name 
getFunNames (FunctionDefn (funcName,_,_,_))
    = removeFunc funcName 

removeFunc :: FuncName -> Name
removeFunc (Custom  name)
    = name 
       


-- ===========================================================
-- ===========================================================

{-
Generate new function names for all the functions
-}

freshFunNames :: [Name] -> State (Int,Int) [(Name,Name)]
freshFunNames [] 
    = return []

freshFunNames (s:ss) = do
    (n,_) <- get 
    let 
      newVar = s ++ "_fun_" ++  show n 
    modify (\(stn,dcare) -> (stn+1,dcare)) 
    restList <- freshFunNames ss 
    return ((s,newVar):restList)  

-- ===========================================================
-- ===========================================================

{-
rename the old functions with their new names.
-}

renameFCall :: [(Name,Name)] -> Term -> Term 
renameFCall subst term 
    = case term of 
        TRecord recBodyL ->
          TRecord (map (handleRecBody subst) recBodyL)

        TCallFun (fName,terms,pn) -> 
          case fName of 
            Custom fn ->
              case lookup fn subst of  
                Just nfn  ->
                  TCallFun (Custom nfn,map (renameFCall subst) terms,pn)
                Nothing -> 
                  term

            BuiltIn _ ->
              TCallFun (fName,map (renameFCall subst) terms,pn) 

        TIf     (t1,t2,t3,pn) -> 
          TIf (
                renameFCall subst t1,
                renameFCall subst t2,
                renameFCall subst t2,pn
              )

        TCase (cterm,pattTerms,pn) ->
          TCase (
                  renameFCall subst cterm,
                  map (\(p,Left pt)-> (p,Left (renameFCall subst pt))) pattTerms,pn
                )

        TFold (fterm,foldPatts,pn) ->
          TFold (
                 renameFCall subst fterm,
                 map (\(n,ps,fTerm,fPn) -> (n,ps,renameFCall subst fTerm,fPn)) foldPatts,
                 pn
                ) 

        TCons   (nm,terms,pn) -> 
          TCons (nm,map (renameFCall subst) terms,pn)

        TDest   (nm,terms,pn) ->
          TDest (nm,map (renameFCall subst) terms,pn)

        TProd   (terms,pn) ->
          TProd (map (renameFCall subst) terms,pn)

        otherwise -> 
          term       

-- =================================================================
-- =================================================================
handleRecBody :: [(Name,Name)] -> (Pattern,Term,PosnPair) -> 
                 (Pattern,Term,PosnPair)

handleRecBody subst (p,t,pn)
    = (p,newT,pn)
  where
    newT = renameFCall subst t 


isFunDefnL :: Defn -> Bool 
isFunDefnL (FunctionDefn _)
    = True 
isFunDefnL _ 
    = False 


