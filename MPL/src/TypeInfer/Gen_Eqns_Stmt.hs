module TypeInfer.Gen_Eqns_Stmt where

import TypeInfer.Gen_Eqns_Seq
import TypeInfer.Gen_Eqns_Defn
import TypeInfer.Gen_Eqns_CommFuns
import TypeInfer.SolveEqns
import TypeInfer.MPL_AST
import TypeInfer.SymTab
import TypeInfer.SymTab_DataType
import TypeInfer.SymTab_Init

import Control.Monad.Except
import Control.Monad.State
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint
import Data.List 


testFunction :: MPL -> IO () 
testFunction mplstmts = do 
    let 
      stEith  = runExceptT (takeCareofMPL mplstmts) 
      eithVal = evalState stEith (1,0,[],[],toBeginSymTab)    
    case eithVal of 
      Left emsg -> 
        putStrLnRed $ unlines
                    [
                      "\n",equalS,equalS,emsg,
                      equalS,equalS
                    ]  

      Right (symtab,tripList)  -> do
        putStrLn $ unlines
                     [equalS,equalS,"See types of all functions or details of one?","\n",
                      "1 for all","\n","Anything else for details of one",equalS,equalS] 
        outChoice <- getLine
        case outChoice of
          "1" -> do 
             putStrLn $ printAllTypes tripList 
            
          otherwise -> do 
             putStrLn "Enter Function number\n"
             funno <- fmap (\x -> (read x)::Int) getLine 
             let 
               partTriple = tripList !! (funno-1)              
             putStrLn $ unlines ["1 for type","2 for Equations","3 for logs"]            
             inchoice <- getLine
             putStrLn $ printType partTriple inchoice


typeMPL :: MPL -> Either String String   
typeMPL mplstmts = do 
    let 
      stEith  = runExceptT (takeCareofMPL mplstmts) 
      eithVal = evalState stEith (1,0,[],[],toBeginSymTab)    
    case eithVal of 
        Left emsg -> 
            Left $ 
              unlines [
                        "\n",equalS,equalS,emsg,equalS,equalS
                      ]  

        Right (symtab,tripList)  -> do
            Right $ printAllTypes tripList 
                

printAllTypes :: [([Defn],Log,[TypeEqn])] -> String 
printAllTypes quadList = concat $ map (\q -> printType q "1") quadList


printType :: ([Defn],Log,[TypeEqn]) -> String -> String
printType (newdefns,log,tEqns) choice 
        = case choice of
              "1" ->  
                  unlines [equalS,"\n",showtypeList,"\n",equalS]

                 where
                    showtypeList 
                      = concat $ map (\x -> getType x ++ "\n") newdefns
                          
              "2" -> 
                  unlines
                     [equalS,equalS,"\n",showEqns,"\n",equalS,equalS]
                  where
                     showEqns = prettyStyle zigStyle tEqns

              otherwise -> do 
                  intercalate "\n" log



{-
First of all find all the data and the codata statements and use it to
form the state. Alos protocol and coprotocol
-}

isDataProtStmt :: Stmt -> Bool 
isDataProtStmt stmt 
        = case stmt of 
              RunStmt _ -> False 
              DefnStmt (defns,stmts,pn) -> 
                  case length stmts == 0 of 
                      True  ->
                           case (head defns) of 
                               Data _     
                                   -> True
                               Codata _
                                   -> True
                               ProtocolDefn _
                                   -> True 
                               CoprotocolDefn _
                                   -> True  
                               otherwise 
                                   -> False
                      False -> 
                           False 


getDataProt :: Stmt -> Defn 
getDataProt (DefnStmt (defns,[],pn)) = head defns

takeCareofMPL :: MPL -> ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                                         (SymbolTable,[([Defn],Log,[TypeEqn])])
takeCareofMPL stmts = do 
        (_,_,_,_,origSTab) <- get 
        let 
          (symTab,remStmts) = updateSymTab  stmts origSTab 
        modify $ \(n,tt,c,chC,st) -> (1,0,[],[],symTab)  
        triples <-  takeCareofStmtList remStmts
        (_,_,_,_,newsTab) <- get 
        return (newsTab,triples)  


updateSymTab :: [Stmt] -> SymbolTable -> (SymbolTable,[Stmt])
updateSymTab stmts origSTab
      = (symTab,remStmts)
  where 
    alldCdstmts
            = filter isDataProtStmt stmts
    allDorProtDefns 
            = map getDataProt alldCdstmts
    symTab  = insert_ST allDorProtDefns origSTab NewScope 
    remStmts= stmts \\ alldCdstmts



takeCareofStmtList :: [Stmt] -> 
                      ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                      [([Defn],Log,[TypeEqn])]

takeCareofStmtList []
        = return []
takeCareofStmtList (s:ss) = do 
        ts  <- takeCareofStmt s 
        tss <- takeCareofStmtList ss 
        return $ (ts ++ tss)

takeCareofStmt :: Stmt -> 
                  ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                  [([Defn],Log,[TypeEqn])]
takeCareofStmt stmt 
    = case stmt of 
        DefnStmt (defns,stmts,pn) ->
          case length stmts == 0 && length defns == 1 of 
              -- this is a simple defn and not a module defn
              True  -> do 
                (_,_,_,_,symTab) <- get 
                quad <- takeCareofDefn (head defns)
                let 
                  (newdefns,log,tEqns) 
                        = quad
                  newST = insert_ST newdefns symTab OldScope
                modify $ \(n,tt,c,chC,st) -> (1,0,[],[],newST)
                return [quad]   
              -- this is module defn. The defns will be visible outside the
              -- defn but the statement will be visible only inside the defn.  
              False -> do 
                (_,_,_,_,oldSymTab) <- get 
                (symTab,quadstmList) <- takeCareofMPL stmts
                let
                   datProtDefns
                       = filter isProtData defns 
                   nDefsStmts 
                       = concat $ map (\(a,b,c) -> a) quadstmList 
                   funSymTab
                       = insert_ST (nDefsStmts ++ datProtDefns) symTab NewScope  
                   funDefns
                       = getallFuns defns
                   remDefns
                       = defns \\ (funDefns ++ datProtDefns)

                modify $ \(n,tt,c,chC,st) -> (1,0,[],[],funSymTab)  
                quadFDefn <- takeCareofFunDefns funDefns 

                let 
                  (finMutDefns,mpackage,mteqns)
                       = quadFDefn 
                  procSymTab
                       = insert_ST finMutDefns funSymTab OldScope
                
                modify $ \(n,tt,c,chC,st) -> (1,0,[],[],procSymTab)
                quadProcDefn <- takeCareofProcDefns remDefns 

                let
                  (finMutProcs,plog,pteqns)
                       = quadProcDefn 
                  finProcDefns 
                       = (\(a,b,c) -> a) quadProcDefn 
                  newSTDefns
                       = insert_ST (finMutDefns ++ finProcDefns ++ datProtDefns)
                                   oldSymTab OldScope  

                modify $ \(n,tt,c,chC,st) -> (1,0,[],[],newSTDefns)
                return [quadFDefn,quadProcDefn]      
                       
        RunStmt (fType,inchs,outchs,process,pn) -> do 
          
          let 
            procDefn
              = ProcessDefn ("main_run",fType,([],inchs,outchs,process),pn)
          triple <- takeCareofDefn procDefn
          return [triple]
          

-- ===============================================================================
-- ===============================================================================
getType :: Defn -> String
getType (FunctionDefn (fname,fType,_,_)) 
        =  show fname ++ " :: " ++ show fType
getType (ProcessDefn (pname,fType,_,_))
        = show pname ++ " :: " ++ show fType


getallFuns :: [Defn] -> [Defn]
getallFuns defns = filter isFunDefn defns 

isFunDefn :: Defn -> Bool
isFunDefn defn = case defn of
        (FunctionDefn _) -> True 
        otherwise        -> False   

getallProcs :: [Defn] -> [Defn]
getallProcs defns = filter isProcDefn defns 

isProcDefn :: Defn -> Bool 
isProcDefn defn = case defn of 
        (ProcessDefn _) -> True
        otherwise       -> False 


