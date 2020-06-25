module ToCMPL.GenAndSolveSetEqns where

import TypeInfer.MPL_AST
import qualified Data.Set as S 
import Data.List 

type FVar = String
type BVar = String

{-
first name is the function name. Every function has some freevars, 
some bound vars and the functions names used in the function body
-}
type SetEqn = (Name,(S.Set BVar,S.Set FVar,[Name]))


{-
At this point all the functions are gonna have only one line of 
pattern term.
-}
genSetEqn :: Defn -> SetEqn 
genSetEqn fDefn@(FunctionDefn (Custom fn,fType,[fBody],pn))
        = (fn,(S.fromList bVars,S.fromList fVars,funDeps)) 
    where
      ((fPatts,Left fTerm),_) 
             = fBody 
      -- get bound vars
      bVars  = getBVarDefn fDefn 
      -- get free vars 
      fVars  = getfreeVarsDefn fDefn 
      funDeps= getFNames fTerm



getBVarDefn :: Defn -> [BVar]
getBVarDefn (FunctionDefn (Custom fn,fType,[fBody],pn))
    = nub $ map (\(VarPattern (u,pn)) -> u) fPatts
  where
      ((fPatts,fTerm),_) = fBody 




{-
get all the free variables used inside a function defintion 
-}

getfreeVarsDefn :: Defn -> [FVar]
getfreeVarsDefn defn@(FunctionDefn (fnm,fType,[fBody],pn))
    = (allVars \\ bvars) 
  where
      ((fPatts,Left fTerm),_) 
             = fBody 
      bvars  = getBVarDefn defn 
      allVars= nub $ getVarsL fTerm


getVarsL :: Term -> [FVar]
getVarsL term 
    = case term of 
        TRecord tList ->
          getVarsL_rec tList

        TCallFun (_,terms,_) ->
          concat $ map getVarsL terms

        TVar    (var,_) -> 
          [var]

        TIf (t1,t2,t3,_) -> 
          (concat.map getVarsL) [t1,t2,t3]

        TCase   (cterm,pattTerms,_) ->
          getVarsL cterm ++ 
          (concat.map getVarsL_PT) pattTerms 

        TFold   (fterm,foldPatts,pn) ->
          getVarsL fterm ++ (termVars \\ pattVars)
          where 
            termVars = (nub.concat.map getVarsL) (map (\(_,_,a,_) -> a) foldPatts)
            pattVars = (concat.map getpattString) (concat(map (\(_,b,_,_) -> b) foldPatts))

        TCons   (_,terms,_) ->
          (concat.map getVarsL) terms

        TDest   (_,terms,_) -> 
          (concat.map getVarsL) terms

        TProd   (terms,_) -> 
          (concat.map getVarsL) terms

        otherwise -> 
          [] 


getVarsL_rec :: [(Pattern,Term,PosnPair)] -> [FVar]
getVarsL_rec tList = nub termVars \\ pattVars
    where
      allterms = map (\(a,b,c) -> b) tList
      allPatts = map (\(a,b,c) -> a) tList
      termVars = (concat.map getVarsL) allterms
      pattVars = (concat.map getpattString) allPatts

{-
At this point all the terms on the right are Left terms.
-}
getVarsL_PT :: PatternTermPhr -> [FVar]
getVarsL_PT (patts,Left term)
    = nub (getVarsL term) \\ bvars 
  where
    bvars = concat $ map getpattString  patts 


getpattString :: Pattern -> [String]
getpattString patt 
    = case patt of 
        ConsPattern (_,patts,_) ->
          (concat.map getpattString) patts 
        DestPattern (_,patts,_) ->
          (concat.map getpattString) patts 
        ProdPattern (patts,_) ->
          (concat.map getpattString) patts 
        VarPattern (var,_) -> [var]
        otherwise -> []

-- ========================================================================
-- ========================================================================

{- get all the function call made inside a term-}  

getFNames ::  Term -> [Name]
getFNames term 
    = case term of 
        TRecord tList ->
           (concat.map getFNames_rec) tList

        TCallFun (fnm,terms,_) ->
          case fnm of 
              Custom sfn ->
                sfn : tnms 
              otherwise ->
                tnms 
           where
             tnms = (concat.map getFNames) terms

        TIf (t1,t2,t3,_) -> 
          (concat.map getFNames) [t1,t2,t3]

        TCase   (term,pattTerms,_) ->
          getFNames term ++ 
          (concat.map getFNames_PT) pattTerms

        TFold   (term,foldPatts,pn) ->
          getFNames term ++ 
          (concat.map getFNames.map (\(a,b,c,d)-> c)) foldPatts

        TCons   (_,terms,_) ->
          (concat.map getFNames) terms

        TDest   (_,terms,_) -> 
          (concat.map getFNames) terms

        TProd   (terms,_) -> 
          (concat.map getFNames) terms

        otherwise -> 
          [] 


getFNames_rec :: (Pattern,Term,PosnPair) -> [Name]
getFNames_rec (_,term,_)
    = getFNames term 

getFNames_PT :: PatternTermPhr -> [Name]
getFNames_PT (patts,Left term)
    = getFNames term 

-- =============================================================================
-- =============================================================================
-- =============================================================================
-- =============================================================================

solveSetEqns :: [SetEqn] -> [(Name,[FVar])]
solveSetEqns seteqns = get_Subst finSetEqns
    where
      -- list of functions which doesn't depend on any function  
      noDepends = filter (\(_,(_,_,nms) )-> nms == []) seteqns 
      -- remaining equations
      remEqns   = seteqns \\ noDepends
      --linearize the remEqns wrt noDepends
      newRemEqs = linWrtConsts (noDepends,remEqns)
      -- systematically linearize the newRemEqs
      linNewRem = linearize_list newRemEqs (length remEqns) 
      finSetEqns= noDepends ++ linNewRem

get_Subst :: [SetEqn] -> [(Name,[FVar])]
get_Subst seqns = map (\(nm,(_,fvs,_)) -> (nm,S.toList fvs)) seqns


linearize_list :: [SetEqn] -> Int -> [SetEqn]
linearize_list setEqns 0 
    = setEqns

linearize_list setEqns num 
    = linearize_list combEqns (num-1)
  where
    befEqns' = linearize eqn befEqns
    aftEqns' = linearize eqn aftEqns
    combEqns = befEqns' ++ (eqn:aftEqns')
    (befEqns,eqn,aftEqns) 
        = mySplit num setEqns



{-
For example 
mySplit 3 [1,2,3,4,5] = ([1,2],3,[4,5])
-}
mySplit :: Int -> [a] -> ([a],a,[a])
mySplit num list 
    = (init l1,last l1,l2)
  where
     (l1,l2) = splitAt num list  

{-
This function will take set equations divided into two parts and linearize
second equation list with respect to the first equation list.
The first element of the pair are the equations that don't depend on other 
functions and second element are the the ones that do. 
-}

linWrtConsts :: ([SetEqn],[SetEqn]) -> [SetEqn]
linWrtConsts ([],fds)
        = fds 
linWrtConsts (n:ns,ds)
        = linWrtConsts (ns,linearize n ds) 


{-
Look at the places where the function name (first argument) in 
the set equation (the first argument) is present in the second argument 
and do the substitution.

linearize the list wrt to the first SetEqn
-}

linearize :: SetEqn -> [SetEqn] -> [SetEqn]
linearize sEqn []
      = []
linearize sEqn setEqns 
      = finSubsEqs ++ nosubsEqs
  where
    (fnm,_)    = sEqn
    -- equations where substitutions can happen
    subsEqns   = filter (funPresent fnm) setEqns
    -- equations where substitutions can't happen
    nosubsEqs  = setEqns \\ subsEqns
    -- equations after substitutions
    finSubsEqs = map (substitute sEqn) subsEqns
    

{-
This function will return a true if the function name which is the first 
argument is present is the fourth argument of the Seteqn.
-}
funPresent :: Name -> SetEqn -> Bool 
funPresent fnm (_,(_,_,names))
    = elem fnm names 



{-
The second SetEqn (set2) has a dependency on a function. The first argument 
(SetEqn set1) is the setEqn corresponding to that. The output is the modified 
SetEqn2. 
-}
substitute :: SetEqn -> SetEqn -> SetEqn
substitute set1 set2 
    = case elem g funs_fin of 
          False ->
            (g,(g_bvars,fvars_fin,funs_fin))  
          True ->
            (g,(g_bvars,fvars_fin,delete g funs_fin)) 
   where
     (f,(f_bvars,f_fvars,f_funs)) 
               = set1 
     (g,(g_bvars,g_fvars,g_funs)) 
               = set2
     -- final free variables
     fvars_fin = S.difference (S.union g_fvars f_fvars) g_bvars  
     funs_fin  = f_funs ++ (delete f g_funs)

-- =================================================================
-- =================================================================
-- =================================================================
-- =================================================================

{-
Once the equations have been solved, the extra arguments need to be added to 
the function definitions and function calls using the solution of the set 
equation.
-}

addExtraArgs_Defn :: [(Name,[FVar])] -> Defn ->  Defn
addExtraArgs_Defn sList (FunctionDefn (fName,fT,pattsList,pn))
      = case fName of 
          Custom nm  ->
            case lookup nm sList of 
              Nothing -> 
                  FunctionDefn (fName,fT,[newPattList],pn)
              Just fvars ->
                  FunctionDefn (fName,fT,[augPattList],pn)
                where
                  augPattList
                    = (((patts ++ map (\x -> VarPattern (x,pn)) fvars),newPTerm),ppn)  

          otherwise  -> 
              FunctionDefn (fName,fT,[newPattList],pn)
  where
    ((patts,Left pTerm),ppn)
      = head pattsList
    newPTerm   
      = Left (addExtArgs_Term sList pTerm)
    newPattList
      = ((patts,newPTerm),ppn)  



addExtArgs_Term :: [(Name,[FVar])] -> Term -> Term 
addExtArgs_Term sList term 
    = case term of 
        TRecord recList ->
            TRecord (map (\(p,t,pn) -> (p,addExtArgs_Term sList t,pn)) recList)

        TCallFun (fName,terms,pn) ->
            case fName of 
              Custom fnm ->
                case lookup fnm sList of 
                  Nothing ->
                    TCallFun (fName,map (addExtArgs_Term sList) terms,pn) 
                  Just fvars -> 
                    TCallFun (fName,terms ++ map (\x -> TVar (x,pn)) fvars,pn)  

              otherwise -> 
                TCallFun (fName,map (addExtArgs_Term sList) terms,pn)  

        TIf (t1,t2,t3,pn) -> 
            TIf (
                  addExtArgs_Term sList t1,
                  addExtArgs_Term sList t2,
                  addExtArgs_Term sList t3,pn
                )

        TCase (term,pattsList,pn) ->
            TCase (
                    addExtArgs_Term sList term,
                    map (\(p,Left pTerm) -> (p,Left (addExtArgs_Term sList pTerm))) pattsList,
                    pn
                  )


        TFold (term,foldPatts,pn) ->
            TFold (
                   addExtArgs_Term sList term,
                   map (\(n,pt,t,p) -> (n,pt,addExtArgs_Term sList t,p)) foldPatts,pn
                  )
            
        TCons (nm,terms,pn) ->
            TCons (nm,map (addExtArgs_Term sList) terms,pn)

        TDest (nm,terms,pn) ->
            TDest (nm,map (addExtArgs_Term sList) terms,pn)

        TProd (terms,pn) ->
            TProd (map (addExtArgs_Term sList) terms,pn)

        otherwise ->
            term


