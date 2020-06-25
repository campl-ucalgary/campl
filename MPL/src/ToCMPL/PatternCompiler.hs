module ToCMPL.PatternCompiler where

import ToCMPL.PatternComp_Help
import ToCMPL.LambdaLift

import TypeInfer.MPL_AST 
import TypeInfer.Gen_Eqns_CommFuns

import TypeInfer.SymTab_DataType
import TypeInfer.SymTab 
import TypeInfer.SymTab_Init


import Control.Monad.State
import Control.Monad.Except
import Data.List 

delFun :: MPL -> Either ErrorMsg MPL 
delFun mpl 
    = case pattCompile mpl of 
        Left emsg -> 
          Left emsg 
        Right iMPL -> 
          Right $ pushTotop iMPL  

-- ================================================================
-- ================================================================

pushTotop :: MPL -> MPL 
pushTotop stmts 
      = [
         pushToTop_help (init stmts) ([],pn),
         last stmts
        ]
  where 
    DefnStmt (_,_,pn) = head stmts 
                 

pushToTop_help :: [Stmt] -> ([Defn],PosnPair) ->  Stmt 
pushToTop_help [] (fDefns,pn)
      = DefnStmt (fDefns,[],pn) 

pushToTop_help (stmt:rest) (iDefns,pn)
      = pushToTop_help rest (iDefns ++ defns ,pn)
  where  
    defns = pushToTop_Stmt stmt

pushToTop_Stmt :: Stmt -> [Defn] 
pushToTop_Stmt (DefnStmt (defns,stmts,_))
    =  (concat $ map pushToTop_Stmt stmts) ++ defns    

-- ================================================================
-- ================================================================

eMsgCase :: (FuncName,PosnPair) -> Term  
eMsgCase (fnm,fpn) 
    = TError $
        "Error in compiling Pattern: Function <<" 
        ++ show fnm ++ ">>" ++ printPosn fpn ++ " is NOT TOTAL.\n"


pattCompile :: MPL -> Either ErrorMsg MPL 
pattCompile mpl = cMPL  
    where 
      newMPL = pushTotop mpl 
      DefnStmt (defns,_,_)
             = head newMPL 
      pdDefns= filter isProtData defns 
      symTab = insert_ST pdDefns toBeginSymTab OldScope  
      stVal  = runExceptT (pattCompile_MPL newMPL)
      cMPL   = evalState stVal (1,0,[],[],symTab) 


pattCompile_MPL :: MPL ->
        ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
                MPL 
pattCompile_MPL stmts = do 
  tStmts <- mapM pattCompile_Stmt stmts
  return tStmts  

pattCompile_Stmt :: Stmt -> 
        ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
                Stmt 
pattCompile_Stmt stmt = do 
    case stmt of 
      DefnStmt (defns,[],pn) -> do 
        newDefns <- mapM (pattCompile_Defn True) defns 
        let finDefns = concat $ map lam_lift_sel newDefns
        return $ DefnStmt (finDefns,[],pn)

      otheriwse ->
        return stmt    


setState :: Bool -> ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) ()  
setState bool 
    = case bool of
          -- reset state   
          True -> do 
            modify $ \(n,a,b,c,d) -> (1,0,b,c,d)  
            return () 
          -- don't reset state 
          False ->
            return () 


pattCompile_Defn :: Bool -> Defn -> 
        ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
                Defn  

pattCompile_Defn bool defn = do 
    (_,_,_,_,symTab) <- get 
    setState bool 
    case defn of 
        FunctionDefn (fName,fType,pairList,pn) -> do 
            let
              pattTerms= map fst pairList 
              ipattT   = map handleNoPatts pattTerms
              numArgs  = (length.fst.head) ipattT
            newArgs <- genNewVarList numArgs
            newPattTerm <- mapM handleEithTerm ipattT
            let 
              strArgs = map (\x -> "fv" ++ show x) newArgs
              varPatts= map (\x -> VarPattern (x,pn)) strArgs
              eithTerm= eMsgCase (fName,pn) 
              match   = MatchFun (fName,pn) strArgs newPattTerm eithTerm 

            newTerm <- normalize_Match match 0

            let 
              newPList= [((varPatts,Left newTerm),pn)]
              newDefn = FunctionDefn (fName,fType,newPList,pn)  

            return newDefn

        otherwise -> 
            return defn 


handleNoPatts :: ([Pattern],Either Term [GuardedTerm]) -> ([Pattern],Either Term [GuardedTerm])
handleNoPatts ([NoPattern _],term) 
    = ([],term)
handleNoPatts pair 
    = pair


{-
This function handles the base case of the normalize_Match function
-}

checkpattList :: (FuncName,PosnPair) ->  [Equation] -> Term -> Int ->
     ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
             Term 

checkpattList (fn,pn) pattTermList defTerm flag = do 
    let
      allPatts  = map fst pattTermList
      nonempty  = filter (\x -> x /= []) allPatts
    case nonempty == [] of 
      True  -> do 
        let 
          term = (snd.head) pattTermList
        case (flag == 0,term) of 
          (True,TError emsg) -> 
            throwError emsg 

          otherwise -> 
            return term

      False -> do 
        case (flag == 0,defTerm) of 
          (True,TError emsg)  ->
              throwError emsg 
             
          otherwise -> 
              return defTerm



normalize_Match :: Match -> Int ->
     ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
             Term
normalize_Match (MatchFun fnPair [] pattTermList defTerm) flag 
    = checkpattList fnPair pattTermList defTerm flag

normalize_Match m@(MatchFun fnPair args pattList defTerm) flag = do 
    (_,_,_,_,symTab) <- get  
    case (ruleType pattList) of 
      0 -> 
        normalize_Match (handleVarPatt m) flag 

      1 -> do
        eqns <- rearrangeEqns pattList fnPair defTerm 
        --error $ show eqns ++ "\n\n" ++ show pattList
        handleConsPatt args fnPair defTerm eqns flag

      otherwise -> do 
        --error $ concat $ map (\x -> show x ++ "\n\n") pattList
        handleMixedPatt args fnPair defTerm pattList




{-
This is the variable case
-}

handleVarPatt :: Match -> Match 
handleVarPatt (MatchFun (fnm,fpn) (u:us) eqns eithVal)
      = MatchFun (fnm,fpn) us (map (varPatt_help u) eqns) eithVal

varPatt_help :: String -> Equation -> Equation
varPatt_help str ((VarPattern pair):ps,term)
      = susbtInPattTerm subst (ps,term)
  where 
    subst  = (TVar pair,TVar (str,snd pair))  


susbtInPattTerm :: (Term,Term) -> Equation -> Equation
susbtInPattTerm subst (patts,term)
    = (patts,(subsInTerm subst term))


{-
This is the case where all the first patterns are constructors.
-}


handleConsPatt :: [String] -> (FuncName,PosnPair) -> Term -> [[Equation]] -> Int ->
     ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
             Term 
handleConsPatt (u:us) fnPair@(fn,fpn) defTerm eqns flag = do 
    pattTermList <- mapM (\eq -> genPattTerm us fnPair defTerm eq flag) eqns
    let 
      term = TCase (TVar(u,fpn),pattTermList,fpn)
    return $ term   
     


{-
This equation takes a list of string and an equation list beginning with
a particular constructor. This is going to be converted to one branch in the
case construct.

-}

genPattTerm :: [String] -> (FuncName,PosnPair) -> Term -> [Equation] -> Int ->
     ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
             PatternTermPhr
genPattTerm us fnPair defTerm eqns@((patts,_):ps) flag = do
    case head patts of 
      ConsPattern (cn,args,pn)  -> do   
          newVars <- genNewVarList (length args)
          let  
            newArgs     = map (\x -> "fv" ++ show x) newVars 
            pattArgs    = map (\x -> VarPattern (x,pn)) newArgs
            newheadPatt = ConsPattern (cn,pattArgs,pn)
            newEqns     = getPattHelper eqns 
            newMatch    = MatchFun fnPair (newArgs++us) newEqns defTerm
          newTerm <- normalize_Match newMatch flag   
          return ([newheadPatt],Left newTerm)

      otheriwse -> 
          error $ show patts   



getPattHelper :: [Equation] -> [Equation]
getPattHelper []
    = []
getPattHelper ((ConsPattern (cn,patts,pn):ps,term):rest)
    = (patts ++ ps,term):getPattHelper rest


-- (MatchFun fnPair args pattList defTerm)

{- This is the mixed case -}
handleMixedPatt :: [String] -> (FuncName,PosnPair) -> Term -> [Equation] ->
     ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
             Term          
handleMixedPatt uvars fnPair defterm [eqn] = do  
    let 
      match = MatchFun fnPair uvars [eqn] defterm
    normalize_Match match 1

handleMixedPatt uvars fnPair defterm (eqn:eqns)  = do 
    let 
      match0 = MatchFun fnPair uvars eqns defterm
    newTerm <- normalize_Match match0 1  
    let 
      match = MatchFun fnPair uvars [eqn] newTerm
    --error $ show (eqn:eqns) 

    normalize_Match match 1


-- ===============================================================
-- ===============================================================
-- I am assuming there is a default term in the guard and at the minimum 
-- there are two guards.
handleEithTerm :: PatternTermPhr -> 
    ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
                ([Pattern],Term)

handleEithTerm (patts,eithTerm) 
    = case eithTerm of 
          Left term -> 
            case term of 
              TIf (t1,t2,t3,_) -> do 
                let 
                  cTerm = genCase (t1,t2) t3 
                return (patts,cTerm)

              TLet _ -> do 
                finTerm <- handleLet term 
                return (patts,finTerm)

              otherwise -> 
                return (patts,term)

          Right gTerms -> do 
            let defTerm = (snd.last) gTerms
            newTerm <- handleGuarded (init gTerms) defTerm
            return (patts,newTerm)

    
{-
   This function will ensure that constants used in the where 
    part of let are replaced with their right hand side in both the term of 
    let term as well as in the where part.

    The point to note here is that to prevent the overwrite of local variables 
    of the let function defintions by the global ones they need to be made 
    refreshed. One way of doing this would be to compile the patterns in these 
    functions. 
-}
handleLet :: Term -> 
    ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
            Term 
handleLet (TLet (lterm,lwhrs,pn)) = do
    let 
      allDefns     = map (\(LetDefn d) -> d) lwhrs 
    newDefns <- mapM (pattCompile_Defn False) allDefns
    let 
      fLetDefn = map (\d -> LetDefn d) newDefns
    return $ TLet (lterm,fLetDefn,pn)

