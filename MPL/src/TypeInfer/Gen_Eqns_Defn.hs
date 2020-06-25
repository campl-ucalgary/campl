module TypeInfer.Gen_Eqns_Defn where

import TypeInfer.Gen_Eqns_Patt
import TypeInfer.Gen_Eqns_Seq
import TypeInfer.Gen_Eqns_Conc
import TypeInfer.Gen_Eqns_CommFuns

import TypeInfer.MPL_AST
import TypeInfer.SymTab
import TypeInfer.SymTab_DataType 
import TypeInfer.SolveEqns

import Control.Monad.State.Lazy
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint
import Data.List 
import Control.Monad.Except
import Data.Maybe

takeCareofDefns :: [Defn] -> 
                   ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                                    [([Defn],Log,[TypeEqn])]
takeCareofDefns []  
        = return []
takeCareofDefns (d:ds) = do 
        q  <- takeCareofDefn d 
        qs <- takeCareofDefns ds 
        return (q:qs)


-- data and codata case will not happen as they have been filtered before
takeCareofDefn :: Defn ->
                 ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                                  ([Defn],Log,[TypeEqn])
takeCareofDefn defn 
        = case defn of 
              TypeSyn (tsyns,pn) -> undefined   
              FunctionDefn _ ->
                  takeCareofFun defn 
              ProcessDefn _ -> 
                  takeCareofProc defn 
              TermSyn _ -> undefined
              OperatorDefn _ -> undefined
              sthg -> error $ "\n" ++ show sthg


takeCareofFun :: Defn -> 
                 ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                                  ([Defn],Log,[TypeEqn])
takeCareofFun defn = do 
        modify $ \(n,tt,c,chC,st) -> (1,0,[],[],st)
        takeCareofFunDefn defn


takeCareofProc :: Defn -> 
                  ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                                  ([Defn],Log,[TypeEqn])
takeCareofProc defn = do 
        modify $ \(n,tt,c,chC,st) -> (1,0,[],[],st)
        takeCareofProcDefn defn   


-- ===========================================================================
-- ===========================================================================

{- This function is tailor made for the mutual case -}
genProcDefnListEqns :: [Defn] -> [TypeThing] -> 
                       ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                       [TypeEqn]
genProcDefnListEqns [] [] 
        = return  []

genProcDefnListEqns (d:ds) (tthing:trest)  = do 
       (_,_,context,_,symTab) <- get 
       modify $ \(n,tt,c,chC,st) -> (n,tthing,c,chC,st)
       dEqns  <- genProcDefnEqns d "mut"  
       modify $ \(n,tt,c,chC,st) -> (n,tt,context,chC,st)
       dsEqns <- genProcDefnListEqns ds trest 
       return $ combineEqns (dEqns++dsEqns)

{-This is the function for the mutual case-}
takeCareofProcDefns :: [Defn] -> 
                       ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                       ([Defn],Log,[TypeEqn])
takeCareofProcDefns [] = do 
         return ([],[],[])

takeCareofProcDefns defns = do
        (startNum,_,_,_,_) <- get  
        typeThings <- genNewVarList (length defns)
        skelVars   <- assignSkelTypes defns
        finEqns    <- genProcDefnListEqns defns typeThings         
        let 
          solEqn  = solveEqns finEqns
        case solEqn of
            Left errormsg -> do 
                throwError $ "Error in mutual processes \n\n" ++
                       intercalate "," (map show (getAllProcNames defns))
                       ++"\n" ++ errormsg 


            Right logpack -> do 
                 let 
                    (log,package)
                            = logpack 
                    (fvars,uvars,evars,subsList)
                            = package    
                 case mkNewProcDefnsMut defns startNum subsList [] of 
                     Left errormsg  ->
                         throwError $ errormsg 
                     Right finDefns ->
                         return (finDefns,log,finEqns)                 
                 

mkNewProcDefnsMut :: [Defn] -> Int -> SubstList -> [Defn] ->
                 Either ErrorMsg [Defn]
mkNewProcDefnsMut [] _ _ finDefns = do 
      return (reverse finDefns)
mkNewProcDefnsMut ((ProcessDefn (fn,ftype,pattProc,pn)):ds) num sList shDefn = do 
      case lookup num sList of 
          Just newFType -> do 
              let
                fvars    = freeVars newFType
                funType  = IntFType (fvars,newFType)
              case intTypeToStrType funType of 
                  Left  errormsg  -> do 
                      Left errormsg
                  Right fnStrType -> do 
                      let 
                        newDefn  = ProcessDefn (fn,fnStrType,pattProc,pn)
                      mkNewProcDefnsMut ds (num+1) sList (newDefn:shDefn)
          Nothing -> do 
             let
               oDefn = ProcessDefn (fn,ftype,pattProc,pn)
             mkNewProcDefnsMut ds (num+1) sList (oDefn:shDefn)




takeCareofProcDefn :: Defn ->
                  ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                                   ([Defn],Log,[TypeEqn])
takeCareofProcDefn defn@(ProcessDefn (pname,mfunType,pattProc,posn)) = do
    procEqns <- genProcDefnEqns defn "norm" 
    let 
      solEqn = solveEqns procEqns
    case mfunType of
        -- no type was given
        NoType ->
          case solEqn of
            Left errormsg -> do 
                let
                   emsg = ["Type error in process <<",show pname, ">> defined ",
                           printPosn posn, "\n", errormsg]
                           --prettyStyle zigStyle procEqns] 

                throwError $ concat emsg 

            Right logpack -> do 
               (_,_,_,_,symTab) <- get  
               let 
                  (log,package)
                           = logpack 
                  (fvars,uvars,evars,subsList)
                           = package                          
                  funType  = IntFType (evars,(snd.head) subsList)
               case intTypeToStrType funType of 
                   Left  iemsg ->
                       throwError $ 
                         unlines 
                         [
                          "Error renaming process name " ++ show pname,
                          iemsg,intercalate "\n" log,
                          prettyStyle zigStyle procEqns
                         ]  

                   Right fnStrType -> do 
                       let 
                         newDefn = ProcessDefn 
                                      (pname,fnStrType,pattProc,posn)

                       modify $ \(n,tt,c,chC,st) -> (1,0,[],chC,st)
                       return ([newDefn],log,procEqns)

        otherwise -> 
            case solEqn of
              Left errormsg -> do 
                  let
                    emsg = "\n" ++ "Type Mistmatch : In process <<" ++
                            show pname ++ ">> defined" ++ printPosn posn
                            ++ "\n\n" ++ errormsg ++ "\n" 
                    errDefn 
                         = ProcessDefn (pname,NoType,pattProc,posn)
                  errEqns <- genProcDefnEqns errDefn "norm" 
                  case solveEqns errEqns of 
                    Left _ ->
                        throwError emsg

                    Right logpack -> do 
                        let 
                          (log,package)
                                   = logpack 
                          (fvars,uvars,evars,subsList)
                                   = package                          
                          funType  = IntFType (evars,(snd.head) subsList)
                        case intTypeToStrType funType of 
                          Left  erremsg   ->
                              throwError $ 
                                 unlines [errormsg,erremsg]

                          Right fnStrType -> do   
                              throwError $ 
                                 concat
                                  [
                                   emsg,
                                   "Expected Type :: " ++ show fnStrType,"\n\n",
                                   "Given Type    :: " ++ show mfunType
                                  ]      

              Right (log,pack) -> do  
                   return ([defn],log,procEqns)

genProcDefnEqns :: Defn -> String -> 
                  ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
                  [TypeEqn]
genProcDefnEqns defn prockind = do 
        (_,_,context,chCont,symTab) <- get
        let 
          ProcessDefn (pname,pType,pattProc,posn)
              = defn 
        newFType <- renameFunType pType
        case prockind == "norm" of 
            True  -> do 
                assignSkelType defn 
                procEqns  <- genPattProcEqns pattProc (pname,posn,newFType)
                modify $ \(n,tt,c,chC,st) -> (n,tt,context,chCont,st)
                return procEqns

            False -> do 
                procEqns  <- genPattProcEqns pattProc (pname,posn,newFType)
                modify $ \(n,tt,c,chC,st) -> (n,tt,context,chCont,st)
                return procEqns

genPattProcEqns :: PattProcessPhr -> (Name,PosnPair,FunType) ->
                  ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
                   [TypeEqn]

genPattProcEqns (patts,inchs,outchs,procs) (pname,pn,fType) = do
    (_,typeProcDefn,_,_,symTab) <- get 
    let 
      procVal= Val_Proc (pname,pn)
      eithVal= lookup_ST procVal symTab 
    case eithVal of
        Left  emsg -> do 
            throwError emsg 

        Right valRet -> do 
            let
              ValRet_Proc (skelFunType,(ns,nls,nrs))
                        = valRet
              IntFType (pars,funType)
                        = skelFunType  
              ProtProc (skelIns,inChsType,outChsType,skelPosn) 
                        = funType 
              lowestEqn = TSimp (TypeVarInt typeProcDefn,funType)
              pattVars  = concat $ map freeVars skelIns
              inChsVars = concat $ map freeVars inChsType
              outChsVars= concat $ map freeVars outChsType
              totVars   = pattVars ++ inChsVars ++ outChsVars

            pattEqns  <- genPattEquationsList patts pattVars
             
            let 
              inCon  = zipWith (\x y -> (x,(In,TypeVarInt y))) inchs inChsVars
              outCon = zipWith (\x y -> (x,(Out,TypeVarInt y))) outchs outChsVars 
              chCont = inCon ++ outCon

            modify $ \(n,tt,c,chC,st) -> (n,tt,c,chCont,st)
            procEqns <- genEquations_Proc procs  
            let 
              combPattProcEqn 
                    = combinePattProcEqns 
                        (combineEqns pattEqns) procEqns 
              totEqn= TQuant ([],totVars) (lowestEqn:combPattProcEqn)

            case fType of
                IntFType _ -> do 
                    let
                      (intFUVars,gseqTypes,ginTypes,goutTypes,sposn)
                          = stripProcProt fType 
                      newProcType
                          = ProtProc (gseqTypes,ginTypes,goutTypes,sposn) 
                      givenEqn 
                          = TQuant (intFUVars,[typeProcDefn])
                              [TSimp (TypeVarInt typeProcDefn,newProcType)]
                      newTotEqn
                          = combineEqns [givenEqn,totEqn]                 
                    return newTotEqn
                  
                otherwise ->
                    return [totEqn]




