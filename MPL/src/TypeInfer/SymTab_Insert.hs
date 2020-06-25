module TypeInfer.SymTab_Insert where

import TypeInfer.MPL_AST
import TypeInfer.Gen_Eqns_CommFuns
import TypeInfer.SymTab_DataType
import TypeInfer.SymTab_Helper 

import Control.Monad.State 
import Data.List 



-- =========================================================================
-- =========================================================================
-- =========================================================================

insert_DataCodata :: [Defn] -> String ->  State ScopeSymbols [Defn]
insert_DataCodata defns flag = do 
        symDefns <- get 
        case flag == "data" of 
              True -> do 
                  let 
                    alldatas = filter isDataDefn defns 
                    remDefns = defns \\ alldatas
                    dataVals = concat $ map compData alldatas
                    dataSym  = filter isData symDefns
                  case dataSym == [] of
                      True  -> do 
                          let 
                            stateData = SymData dataVals
                          modify $ \ssyms -> stateData:ssyms
                          return remDefns    
                      False -> do 
                          let 
                            oldSymVals 
                                = removeCons (head dataSym) 
                            stateData
                                = SymData (oldSymVals++dataVals)     
                          modify $ \ssyms -> stateData:ssyms
                          return remDefns 
              False -> do             
                  let 
                    allCodatas = filter isCodataDefn defns 
                    remDefns   = defns \\ allCodatas
                    codataVals = concat $ map compCoData allCodatas
                    codataSym  = filter isCodata symDefns
                  case codataSym == [] of
                      True  -> do 
                          let 
                            stateCodata = SymCodata codataVals
                          modify $ \ssyms -> stateCodata:ssyms
                          return remDefns    
                      False -> do 
                          let 
                            oldSymVals 
                                = removeDest (head codataSym) 
                            stateCodata
                                = SymCodata (oldSymVals++codataVals)     
                          modify $ \ssyms -> stateCodata:ssyms
                          return remDefns 

-- *************************************************************************
removeDataCodata :: Defn -> [DataClause]
removeDataCodata (Data dpairs) 
    = fst dpairs
removeDataCodata (Codata cdpairs)
    = fst cdpairs     

-- *************************************************************************
compData :: Defn -> [(Name,ConsVal)]
compData (Data dclsposn) 
        = evalState (handleData dclsposn) 0

-- *************************************************************************
handleData :: ([DataClause],PosnPair) -> State Int [(Name,ConsVal)]
handleData (dcls,posn) = do
      allPairs <- handleDataHelper dcls posn  
      let 
        allconses = getMutConses allPairs []
        finPairs  = map (changeConsVal allconses) allPairs
      return finPairs


handleDataHelper :: [DataClause] -> PosnPair ->  State Int [(Name,ConsVal)]
handleDataHelper [] _    = return []
handleDataHelper (d:ds) posn = do 
        d'  <- handleDataClause d posn 
        ds' <- handleDataHelper ds posn 
        return $ d' ++ ds'


changeConsVal :: [Name] -> (Name,ConsVal) ->  (Name,ConsVal)
changeConsVal finConses (nm,((dname,conses),fun,foldFun,nargs)) 
        = (nm,((dname,finConses),fun,foldFun,nargs))

-- here we are getting the conses of the other mutually recursive types.
getMutConses :: [(Name,ConsVal)] -> [Name] -> [Name]
getMutConses [] finNames
        = nub finNames
getMutConses ((n,((_,conses),_,_,_)):rest) currCons 
        = getMutConses rest  (conses ++ currCons)
-- *************************************************************************

handleDataClause :: DataClause -> PosnPair -> State Int [(Name,ConsVal)]            
handleDataClause (datname,dataphrs) posn = do 
      str <- genString 
      let 
        datFun    = dNametoDTypeFun datname posn 
        allconses = map getConsName dataphrs
        list = handleDClauseHelp dataphrs (datname,allconses) str datFun
      return list 


handleDClauseHelp :: [DataPhrase] -> (DataName,[Name]) -> String -> FunType ->
                     [(Name,ConsVal)]
handleDClauseHelp dphrs pair str datFType  
        = map (\d -> dclauseCons d pair str datFType) dphrs 

-- *************************************************************************

dclauseCons :: DataPhrase -> (DataName,[Name]) -> String -> FunType -> (Name,ConsVal)
dclauseCons (consName,consType,nargs) d@(datName,allconses) str datFType
        = let 
            foldType = genFoldFun consType (datName,str) consName
          in 
            (consName,(d,consType,(foldType,datFType),nargs))  

dNametoDTypeFun :: DataName -> PosnPair -> FunType 
dNametoDTypeFun (DataName (dname,args)) dposn
        = StrFType (tVars,dType)
    where
      dType = TypeDataType (dname,strToTVar args dposn,dposn)
      tVars = getParamVars dType

          
genFoldFun :: FunType -> (DataName,String) -> Name -> FunType
genFoldFun (StrFType (univs,fType)) (dname,str) consName
        = StrFType (tVars,newFun)
    where
       newFun = renameCons (dname,str) fType
       tVars  = getParamVars newFun

genString :: State Int String 
genString = do  
        num <- get 
        modify $ \n -> n + 1
        return $ "_V" ++ show num 

checkDataType :: DataName -> Type -> Bool
checkDataType (DataName (oname,args)) (TypeDataType (dname,types,pn))  
        = case cond1 of 
              True -> cond2 && cond3
              otherwise -> True
    where
       cond1 = dname == oname
       cond2 = length args == length types
       cond3 = checkArgs args types

checkArgs :: [String] -> [Type] -> Bool
checkArgs args datArgs 
        = args == (map removeTVar datArgs) 

removeTVar :: Type -> String 
removeTVar (TypeVar (v,_)) = v

renameCons ::(DataName,String) -> Type -> Type 
renameCons orig@(DataName (oname,args),str) sType
        = case sType of 
              TypeDataType (dname,types,pn) ->
                  case oname == dname of 
                      True  ->
                         case checkDataType (DataName (oname,args)) sType of 
                             True  -> 
                                 TypeVar (str,pn)
                             False -> 
                                 error $
                                    "Error defining data type <<" ++ oname ++ 
                                    ">> or error in the type of constructor \n"
                      False -> 
                         TypeDataType (
                                        dname,
                                        map (renameCons orig) types,
                                        pn
                                      ) 

 

              TypeCodataType (cdName,types,pn) ->
                  TypeCodataType (
                                   cdName,
                                   map (renameCons orig) types ,
                                   pn
                                 ) 

              TypeProd (types,pn) ->
                  TypeProd (
                            map (renameCons orig ) types, 
                            pn
                           )
              TypeFun (types,stype,pn) ->
                  TypeFun (
                            map (renameCons orig ) types,
                            renameCons orig stype,
                            pn
                          )

              otherwise -> 
                  sType 


-- =========================================================================
-- =========================================================================

-- *************************************************************************
compCoData :: Defn -> [(Name,DestVal)]
compCoData = handleCodata.removeDataCodata

-- *************************************************************************
handleCodata :: [DataClause] -> [(Name,DestVal)]
handleCodata dcls  
        = concat $ map handleCoDataClause dcls 

-- *************************************************************************

handleCoDataClause :: DataClause -> [(Name,DestVal)]            
handleCoDataClause (cdname,dataphrs)
        = map (\d -> dclauseDest d (cdname,alldests)) dataphrs
    where
      alldests 
          = map getConsName dataphrs

getConsName :: DataPhrase -> Name
getConsName (name,_,_) = name

-- *************************************************************************

dclauseDest :: DataPhrase -> (DataName,[Name]) -> (Name,DestVal)
dclauseDest (consName,consType,nargs) datName 
        = (consName,(datName,consType,nargs))      

-- =========================================================================
-- =========================================================================
-- =========================================================================
insert_ProtCoprot :: [Defn] -> String ->  State ScopeSymbols [Defn]
insert_ProtCoprot defns flag = do 
        symDefns <- get 
        case flag == "prot" of 
              True -> do 
                  let 
                    allprots = filter isProtDefn defns 
                    remDefns = defns \\ allprots
                    protVals = concat $ map compProtCoProt allprots
                    protSym  = filter isProt symDefns
                  case protSym == [] of
                      True  -> do 
                          let 
                            stateProt = SymProt protVals
                          modify $ \ssyms -> stateProt:ssyms
                          return remDefns    
                      False -> do 
                          let 
                            (oldSymVals,_) 
                                = removeHandCohand (head protSym) 
                            stateProt
                                = SymProt (oldSymVals++protVals)     
                          modify $ \ssyms -> stateProt:ssyms
                          return remDefns 
              False -> do             
                  let 
                    allCoprots = filter isCoProtDefn defns 
                    remDefns   = defns \\ allCoprots
                    coprotVals = concat $ map compProtCoProt allCoprots
                    coprotSym  = filter isCoProt symDefns
                  case coprotSym == [] of
                      True  -> do 
                          let 
                            statecoProt = SymCoProt coprotVals
                          modify $ \ssyms -> statecoProt:ssyms
                          return remDefns    
                      False -> do 
                          let 
                            (oldSymVals,_) 
                                = removeHandCohand (head coprotSym) 
                            statecoProt
                                = SymCoProt (oldSymVals++coprotVals)     
                          modify $ \ssyms -> statecoProt:ssyms
                          return remDefns 

compProtCoProt :: Defn -> [(Name,HandVal)]
compProtCoProt = handleProtCoProt.removeProtCoprot

-- *************************************************************************

handleProtCoProt :: [ProtocolClause] -> [(Name,HandVal)]
handleProtCoProt pcls  
        = concat $ map handleProtClause pcls 

-- *************************************************************************

handleProtClause :: ProtocolClause -> [(Name,HandVal)]            
handleProtClause (pname,protphrs)
        = map (\p -> protclauseHelper p pname) protphrs

-- *************************************************************************

protclauseHelper :: ProtocolPhrase -> ProtName -> (Name,HandVal)
protclauseHelper pair protname 
        =  (fst pair,(protname,snd pair) )

-- *************************************************************************

removeProtCoprot :: Defn -> [ProtocolClause]
removeProtCoprot (ProtocolDefn ppairs) 
    = fst ppairs 
removeProtCoprot (CoprotocolDefn cppairs)
    = fst cppairs 

-- =========================================================================
-- =========================================================================
-- =========================================================================

insert_TypeSyn :: [Defn] -> State ScopeSymbols [Defn]
insert_TypeSyn defns = do 
        symDefns <- get 
        let 
          allSyms  = filter isTypeSynDefn defns
          remDefns = defns \\  allSyms
          symVals  = concat $ map removeTypeSynDefn allSyms
          typeSynVal
                   = filter isTypeSyn symDefns
        case typeSynVal == [] of 
            True  -> do 
               let 
                 stateSyn = SymTypeSyn symVals
               modify $ \ssyms -> stateSyn:ssyms   
               return remDefns
                
            False -> do
               let 
                 oldSymVals 
                          = removeSymType (head typeSynVal)           
                 stateSyn = SymTypeSyn (oldSymVals ++ symVals)
               modify $ \ssyms -> stateSyn:ssyms   
               return remDefns


removeTypeSynDefn :: Defn -> [TypeSynonym]
removeTypeSynDefn (TypeSyn tpairs)
    = fst tpairs

-- *************************************************************************

  

-- =========================================================================
-- =========================================================================
-- =========================================================================

insert_FunMutFun :: [Defn] -> State ScopeSymbols [Defn]
insert_FunMutFun defns = do 
        symDefns <- get 
        let 
          allFuns  = filter isFunDefn defns 
          remDefns = defns \\ allFuns
          symVals  = (map removeFunDefn allFuns) 
          funVal   = filter isFun symDefns
        case funVal == [] of 
            True  -> do 
               let 
                 stateFun= SymFun symVals
               modify $ \ssyms -> stateFun:ssyms   
               return remDefns
                
            False -> do
               let 
                 oldSymVals 
                          = removeSymFun (head funVal)   
                 --stateFun = SymFun (oldSymVals)        
                 stateFun = SymFun (symVals++oldSymVals)
               modify $ \ssyms -> stateFun:(ssyms \\funVal)    
               return remDefns



removeFunDefn :: Defn -> (FuncName,(FunType,NumArgs))
removeFunDefn (FunctionDefn (fname,fType,flist,_))
    = (fname,(fType,(length.fst.fst.head) flist))

-- =========================================================================
-- =========================================================================
insert_ProcMutProc :: [Defn] -> State ScopeSymbols [Defn]
insert_ProcMutProc defns = do 
        symDefns <- get 
        let 
          allProcs  = filter isProcDefn defns 
          remDefns  = defns \\ allProcs
          symVals   = (map removeProcDefn allProcs) 
          procVal   = filter isProc symDefns
        case procVal == [] of 
            True  -> do 
               let 
                 stateProc = SymProc symVals
               modify $ \ssyms -> stateProc:ssyms   
               return remDefns
                
            False -> do
               let 
                 oldSymVals
                           = removeSymProc (head procVal)           
                 stateProc = SymProc (oldSymVals ++ symVals)
               modify $ \ssyms -> stateProc:ssyms   
               return remDefns
 
   
               
removeProcDefn :: Defn -> (Name,(FunType,(NumArgs,NumArgs,NumArgs)))
removeProcDefn (ProcessDefn (pname,ptype,pattproc,_))
        = (pname,(ptype,(l1,l2,l3)))                   
    where
          (l1,l2,l3) = getThreeLengths pattproc
          getThreeLengths :: PattProcessPhr -> (Int,Int,Int)
          getThreeLengths (patts,ins,os,etrem)
              = (length patts,length ins,length os)

   

-- =========================================================================
-- =========================================================================

updFTypeSymTab :: (FuncName,FunType) -> [ScopeSymbols] -> [ScopeSymbols] ->
                  Either ErrorMsg [ScopeSymbols]
updFTypeSymTab (fname,_) [] _ =
          Left $ "Function <<" ++ show fname ++ ">> which was to be updated with a new type"
                 ++ "was not found in symbol table.\n"

updFTypeSymTab (fname,fType) (ssyms:rest) shuntsyms = do 
          let 
            msymDefn = getSymFun ssyms 
          case msymDefn of 
              Just (symFun,restSyms) -> do 
                  let 
                    funList = removeSymFun symFun
                  case lookup fname funList of 
                      Just (oldType,nargs) -> do 
                          let 
                            remPrevFun  = delete (fname,(oldType,nargs)) funList
                            newFunWTyp  = remPrevFun ++ [(fname,(fType,nargs))]
                            newssysms   = (SymFun newFunWTyp):restSyms 
                            finScopeSyms= reverse shuntsyms ++ 
                                          (newssysms:rest)
                          return finScopeSyms                      
                      Nothing ->  
                          updFTypeSymTab (fname,fType) rest (ssyms:shuntsyms) 
              Nothing -> do 
                  updFTypeSymTab (fname,fType) rest (ssyms:shuntsyms)   
    


getSymFun :: ScopeSymbols -> Maybe (SymbolDefn,ScopeSymbols) 
getSymFun sdefns = do 
        let 
          msymFun = (filter isFun sdefns)
        case msymFun == [] of 
            False -> do 
                let remsymDefns = sdefns \\ msymFun
                return (head msymFun,remsymDefns)
            True ->
                Nothing  


          