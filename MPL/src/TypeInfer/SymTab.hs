module TypeInfer.SymTab where

import TypeInfer.MPL_AST
import TypeInfer.Gen_Eqns_CommFuns
import TypeInfer.SymTab_DataType
import TypeInfer.SymTab_Helper
import TypeInfer.SymTab_Insert

import Control.Monad.State
import Control.Monad.Except

-- This will find out whether there is a data/codata/protocol/coprotocol/fun/process
-- present in the symbol table. If it is return the value else the error message
lookup_ST :: ValLookup -> SymbolTable -> Either ErrorMsg ValRet
lookup_ST val symTab = do 
        let symDefns = concat symTab
        case val of 
              Val_Cons (cname,posn) -> do 
                  let 
                    symData = filter isData symDefns  
                  lookup_Cons (cname,posn) symData 

              Val_Dest (dname,posn) -> do 
                  let 
                    symCodata = filter isCodata symDefns 
                  lookup_Dest (dname,posn) symCodata

              Val_Prot (hname,posn) -> do 
                  let 
                    symProt = filter isProt symDefns
                  lookup_HandCohand (hname,posn) symProt

              Val_Coprot (chname,posn) -> do 
                  let 
                    symCoProt = filter isCoProt symDefns
                  lookup_HandCohand (chname,posn) symCoProt

              Val_TypeSyn (tsynm,posn) -> do 
                  let 
                     symTypeSyn = filter isTypeSyn symDefns
                  lookup_TypeSyn (tsynm,posn) symTypeSyn    

              Val_Fun (fname,posn) -> do 
                  let 
                    symFun = filter isFun symDefns
                  lookup_Fun (fname,posn) symFun   

              Val_Proc (pname,posn) -> do 
                  let 
                    symProc = filter isProc symDefns
                  lookup_Proc (pname,posn) symProc

 
-- =====================================================================================
-- =====================================================================================

-- Here the first argument is the constructor/destructor name with posn and 
-- second is the list of all data defintions that should be visible.
lookup_Cons :: (Name,PosnPair) -> [SymbolDefn] -> Either ErrorMsg ValRet
lookup_Cons (name,posn) datas = do 
        let 
          emsg = "No data defintion found for constructor <<"
                 ++ show name  ++ ">> used" ++ printPosn posn 

        case datas /= [] of 
            True  -> do
                let 
                  consVals
                      = concat $ map removeCons datas   
                case lookup name consVals of 
                    Just consVal ->  
                        return $ ValRet_Cons consVal

                    Nothing ->  
                        Left emsg      
                          
            False -> do 
                Left emsg 

  

lookup_Dest :: (Name,PosnPair) -> [SymbolDefn] -> Either ErrorMsg ValRet
lookup_Dest (name,posn) codatas = do 
        let 
          emsg = "No codata defintion found for destructor <<"
                 ++ show name  ++ ">> used" ++ printPosn posn 

        case codatas /= [] of 
            True  -> do
                let 
                  destVals
                      = concat $ map removeDest codatas                         
                case lookup name destVals of 
                    Just destVal ->  
                        return $ ValRet_Dest destVal   
                    Nothing ->  
                        Left emsg      
                          
            False -> do 
                Left emsg 


-- =====================================================================================
-- =====================================================================================

lookup_HandCohand :: (Name,PosnPair) -> [SymbolDefn] -> Either ErrorMsg ValRet
lookup_HandCohand (name,posn) handCohands = do 
        let 
          emsg = "No protocol/coprotocol defintion found for handle/cohandle <<"
                 ++ show name  ++ ">> defined " ++ printPosn posn 

        case handCohands /= [] of 
            True  -> do
                let 
                  allPairs
                      = map removeHandCohand handCohands
                  handOrCohand
                      = (snd.head) allPairs 
                  onlyHandVals
                      = (concat.map fst) allPairs      
                case lookup name onlyHandVals of 
                    Just handVal ->  
                        case handOrCohand == "handle" of
                            True  ->  
                                return $ ValRet_Prot handVal
                            False ->
                                return $ ValRet_Coprot handVal
                    Nothing ->  
                        Left emsg      
            False ->  
                Left emsg                  


-- =====================================================================================
-- =====================================================================================
lookup_Fun :: (FuncName,PosnPair) -> [SymbolDefn] -> Either ErrorMsg ValRet
lookup_Fun (name,(line,col)) funDefns = do 
        let 
          emsg = "Function <<" ++ show name ++ ">>  used at (" ++ show line ++ "," ++
                 show col ++ ") not in scope.\n" 
        case funDefns /= [] of
            True  ->  do 
                let 
                  fdefns 
                      = concat $ map removeSymFun funDefns
                case lookup name fdefns of 
                    Just pairTypeArgs -> do 
                        return $ ValRet_Fun pairTypeArgs
                    Nothing -> do   
                        Left emsg              
            False -> 
                Left emsg  

                       
-- =====================================================================================
-- =====================================================================================
lookup_Proc :: (Name,PosnPair) -> [SymbolDefn] -> Either ErrorMsg ValRet
lookup_Proc (name,(line,col)) procDefns = do 
        let 
          emsg = "Process <<" ++ show name ++ ">>  used at (" ++ show line ++ "," ++
                 show col ++ ") not in scope.\n"  
        case procDefns /= [] of
            True  ->  do 
                let 
                  pdefns 
                      = concat $ map removeSymProc procDefns
                case lookup name pdefns of 
                    Just pairTypeArgs -> do 
                        return $ ValRet_Proc pairTypeArgs
                    Nothing -> do   
                        Left emsg              
            False -> 
                Left emsg  

                       
-- =====================================================================================
-- =====================================================================================

lookup_TypeSyn :: (Name,PosnPair) -> [SymbolDefn] ->  Either ErrorMsg ValRet
lookup_TypeSyn (name,(line,col)) typeDefns = do 
        let 
          emsg = "Type synonym <<" ++ show name ++ ">>  used at (" ++ show line ++ "," ++
                 show col ++ ") not in scope.\n"          
        case typeDefns == [] of 
            True  -> do 
                let 
                  tdefns = concat $ map removeSymType typeDefns
                case lookup name tdefns of 
                    Just tsynm -> do 
                        return $ ValRet_TypeSyn tsynm
                    Nothing ->
                        Left emsg 
            False ->
                Left emsg         

-- =====================================================================================
-- =====================================================================================
insert_ST :: [Defn] -> SymbolTable -> ScopeType ->  SymbolTable
insert_ST defns symTab stype 
        = case stype of 
              OldScope ->
                  (snew:(tail symTab))
                where
                    snew = execState (insert_helper defns) (head symTab)  
              NewScope -> 
                  (snew:symTab)
                where
                    snew = execState (insert_helper defns) []  

insert_helper :: [Defn] -> State ScopeSymbols ()
insert_helper []
        = return ()
insert_helper defns@(d:ds) = do 
        case d of 
            Data _ -> do 
               remDefns <- insert_DataCodata defns "data"
               insert_helper remDefns 
            Codata _  -> do 
               remDefns <- insert_DataCodata defns "codata"
               insert_helper remDefns 
            TypeSyn _ -> do    
               remDefns <- insert_TypeSyn defns
               insert_helper remDefns                
            ProtocolDefn _ -> do 
               remDefns <- insert_ProtCoprot defns "prot"
               insert_helper remDefns 
            CoprotocolDefn _ -> do 
               remDefns <- insert_ProtCoprot defns "coprot"
               insert_helper remDefns 
            FunctionDefn _ -> do 
               remDefns <- insert_FunMutFun defns
               insert_helper remDefns 
            ProcessDefn  _ -> do 
               remDefns <- insert_ProcMutProc defns
               insert_helper remDefns 


-- =====================================================================================
-- =====================================================================================
{-
-- This function just updates the function type. At some point this function 
-- needs to take care of process types as well. 
update_ST :: Defn -> SymbolTable -> Either ErrorMsg SymbolTable
update_ST defn stab = do 
        case defn of
            FunctionDefn (fname,newFType,_,_) -> 
                    updFTypeSymTab (fname,newFType) stab []
            ProcessDefn _ ->
                    undefined


update_ST_List :: [Defn] -> SymbolTable -> Either ErrorMsg SymbolTable
update_ST_List [] symTab 
        = return symTab
update_ST_List (d:ds) symTab = do 
        newsymTab  <- update_ST  d symTab
        finSymTab  <- update_ST_List ds newsymTab
        return finSymTab
-}
