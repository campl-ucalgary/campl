module MPLPar.PtreeToAST where

import MPLPar.AbsMPL
import MPLPar.PtreeToAST_help

import qualified TypeInfer.MPL_AST as M 
import qualified TypeInfer.Gen_Eqns_CommFuns as E 

import Control.Monad.State.Lazy
import qualified Data.Set as S 
import Data.List

transTokUnit :: TokUnit -> M.PosnPair
transTokUnit x = case x of 
   TokUnit string -> fst string  

transTokSBrO :: TokSBrO -> M.PosnPair
transTokSBrO x = case x of
   TokSBrO str -> fst str

transTokSBrC :: TokSBrC -> M.PosnPair
transTokSBrC x = case x of
   TokSBrC str -> fst str

transTokDefn :: TokDefn -> M.PosnPair
transTokDefn x = case x of
  TokDefn string -> fst string

transTokRun :: TokRun -> M.PosnPair
transTokRun x = case x of
  TokRun string -> fst string

transTokTerm :: TokTerm -> M.PosnPair
transTokTerm x = case x of
  TokTerm string -> fst string

transTokData :: TokData -> M.PosnPair
transTokData x = case x of
  TokData string -> fst string

transTokCodata :: TokCodata -> M.PosnPair
transTokCodata x = case x of
  TokCodata string -> fst string

transTokType :: TokType -> M.PosnPair
transTokType x = case x of
  TokType string -> fst string

transTokProtocol :: TokProtocol -> M.PosnPair
transTokProtocol x = case x of
  TokProtocol string -> fst string

transTokCoprotocol :: TokCoprotocol -> M.PosnPair
transTokCoprotocol x = case x of
  TokCoprotocol string -> fst string

transTokGetProt :: TokGetProt -> M.PosnPair
transTokGetProt x = case x of
  TokGetProt string -> fst string

transTokPutProt :: TokPutProt -> M.PosnPair
transTokPutProt x = case x of
  TokPutProt string -> fst string

transTokNeg :: TokNeg -> M.PosnPair
transTokNeg x = case x of
  TokNeg string -> fst string

transTokTopBot :: TokTopBot -> M.PosnPair
transTokTopBot x = case x of
  TokTopBot string -> fst string

transTokFun :: TokFun -> M.PosnPair
transTokFun x = case x of
  TokFun string -> fst string

transTokDefault :: TokDefault -> M.PosnPair
transTokDefault x = case x of
  TokDefault string -> fst string

transTokRecord :: TokRecord -> M.PosnPair
transTokRecord x = case x of
  TokRecord string -> fst string

transTokIf :: TokIf -> M.PosnPair
transTokIf x = case x of
  TokIf string -> fst string

transTokFold :: TokFold -> M.PosnPair
transTokFold x = case x of
  TokFold string -> fst string

transTokUnfold :: TokUnfold -> M.PosnPair
transTokUnfold x = case x of
  TokUnfold string -> fst string

transTokCase :: TokCase -> M.PosnPair
transTokCase x = case x of
  TokCase string -> fst string

transTokProc :: TokProc -> M.PosnPair
transTokProc x = case x of
  TokProc string -> fst string

transTokClose :: TokClose -> M.PosnPair
transTokClose x = case x of
  TokClose string -> fst string

transTokHalt :: TokHalt -> M.PosnPair
transTokHalt x = case x of
  TokHalt string -> fst string

transTokGet :: TokGet -> M.PosnPair
transTokGet x = case x of
  TokGet string -> fst string

transTokPut :: TokPut -> M.PosnPair
transTokPut x = case x of
  TokPut string -> fst string

transTokHCase :: TokHCase -> M.PosnPair
transTokHCase x = case x of
  TokHCase string -> fst string

transTokHPut :: TokHPut -> M.PosnPair
transTokHPut x = case x of
  TokHPut string -> fst string

transTokSplit :: TokSplit -> M.PosnPair
transTokSplit x = case x of
  TokSplit string -> fst string

transTokFork :: TokFork -> M.PosnPair
transTokFork x = case x of
  TokFork string -> fst string

transUIdent :: UIdent -> (M.PosnPair,String)
transUIdent x = case x of
  UIdent string -> string

transPIdent :: PIdent -> (M.PosnPair,String)
transPIdent x = case x of
  PIdent string -> string

{-
transInfixRem :: InfixRem -> (M.PosnPair,String)
transInfixRem x = case x of
  InfixRem string -> string
-}


transPInteger :: PInteger -> (M.PosnPair,Int)
transPInteger x = case x of 
  PInteger (posn,integer) -> (posn,read integer::Int)  

transTokDCare :: TokDCare -> M.PosnPair
transTokDCare x = case x of 
  TokDCare (posn,str) -> posn  


transMPL :: MPL -> State [(String,[M.Name])] [M.Stmt]
transMPL x = case x of
  MPLPROG mplstmts runstmt -> do 
      stmts1 <- mapM transMPLstmt mplstmts    
      rstmt1 <- transRUNstmt runstmt
      return (stmts1 ++ [rstmt1])

transInfix0op :: Infix0op -> M.FuncName
transInfix0op x = case x of
  Infix0op string -> detectFun "orB" 

transInfix1op :: Infix1op -> M.FuncName
transInfix1op x = case x of
  Infix1op string -> detectFun "andB"

transInfix2op :: Infix2op -> M.FuncName
transInfix2op x = case x of
  Infix2op string -> case string of 
        "==" -> detectFun "eqI"
        "/=" -> detectFun "notEqI"
        "<"  -> detectFun "leqI"
        ">"  -> detectFun "geqI" 
        "<=" -> detectFun "leqI"
        ">=" -> detectFun "geqI"
        otherwise -> error $ "wrong symbol::" ++ string
      

transInfix3op :: Infix3op -> M.FuncName
transInfix3op x = case x of
  Infix3op string -> detectFun "append"

transInfix4op :: Infix4op -> M.FuncName
transInfix4op x = case x of
  Infix4op string -> case string of 
      "+" -> detectFun "addI"
      "-" -> detectFun "subI"
      otherwise -> error $ "wrong symbol::" ++ string

transInfix5op :: Infix5op -> M.FuncName
transInfix5op x = case x of
  Infix5op string -> case string of 
      "*"    -> detectFun "mulI"
      "/"    -> detectFun "quotI"
      "%"    -> detectFun "remI"
      "div"  -> detectFun "quotI"
      "rem"  -> detectFun "remI"
      "quot" -> detectFun "quotI"
      otherwise -> error $ "wrong symbol::" ++ string

transInfix6op :: Infix6op -> M.FuncName
transInfix6op x = case x of
  Infix6op string -> case string of
      "^" -> detectFun "powI" 
      otherwise -> error $ "wrong symbol::" ++ string

transInfix7op :: Infix7op -> M.FuncName
transInfix7op x = case x of
  Infix7op string -> case string of
      "!!" -> detectFun "index" 
      otherwise -> error $ "wrong symbol::" ++ string


transMPLstmt :: MPLstmt -> 
                State [(String,[M.Name])] M.Stmt

transMPLstmt x = case x of
  WHEREDEFN tokdefn defns mplstmtalts -> do 
      tDefns    <- mapM transDefn defns
      tMPLStmts <- mapM transMPLstmtAlt mplstmtalts
      return $ M.DefnStmt (tDefns,tMPLStmts,transTokDefn tokdefn)

  WOWHEREDEFN tokdefn defns -> do 
      tDefns <- mapM transDefn defns
      return $ M.DefnStmt (tDefns,[],transTokDefn tokdefn)

  BAREDEFN defn -> do 
      tDefn <- transDefn defn 
      return $ M.DefnStmt ([tDefn],[],E.getDefnPosn tDefn)

-- ================================================================================

transMPLstmtAlt :: MPLstmtAlt -> 
                   State [(String,[M.Name])] M.Stmt

transMPLstmtAlt x = case x of
    MPLSTMTALT mplstmt -> do 
        transMPLstmt mplstmt

-- ================================================================================

transRUNstmt :: RUNstmt -> State [(String,[M.Name])] M.Stmt
transRUNstmt x = case x of
  RUNSTMTWITHType tokrun protocols1 protocols2 channels1 channels2 process -> do  
      tProt1 <- mapM transProtocol protocols1
      tProt2 <- mapM transProtocol protocols2
      let 
        tChs1  = map transChannel channels1
        tChs2  = map transChannel channels2
        runType= M.ProtProc (
                              [],tProt1,tProt2,
                              E.getTypePosn (head tProt1)
                            )
        rvars  = E.getParamVars runType
        fType  = M.StrFType (rvars,runType)

      tProc <- transProcess process
      return $  
          M.RunStmt (
                      fType,
                      tChs1,tChs2,
                      tProc,transTokRun tokrun
                    )

  RUNSTMTWITHTOUType tokrun channels1 channels2 process -> do 
      let 
        tChs1 = map transChannel channels1
        tChs2 = map transChannel channels2
      tProc <- transProcess process 
      return $
          M.RunStmt
              (
                  M.NoType,  
                  tChs1,tChs2, 
                  tProc,transTokRun tokrun
              )
     
-- ================================================================================

transDefn :: Defn -> State [(String,[M.Name])] M.Defn 
transDefn x = case x of
  TYPEDEF typedefn -> 
      transTypeDefn typedefn 

  PROTOCOLDEF ctypedefn -> 
      transCTypeDefn ctypedefn

  FUNCTIONDEF functiondefn -> do 
      let
         nmList = getFunctionDefnName functiondefn
      dupCheck <- insertNamesInList ("function",nmList)
      case dupCheck of 
          Nothing -> 
              transFunctionDefn functiondefn
          Just errormsg -> 
              error $ unlines
                [
                  "\n",E.equalS,E.equalS,"*********Error*********",
                  errormsg,E.equalS,E.equalS
                ]

  --OPERATORDEF operatordefn -> 
  --    return $ transOperatorDefn operatordefn

  PROCESSDEF processdef -> do 
      let 
        nmList = getProcDefnName processdef 
      dupCheck <- insertNamesInList ("process",nmList)
      case dupCheck of 
          Nothing -> 
              transProcessDef processdef
          Just errormsg -> 
              error $ unlines 
                [
                 "\n",E.equalS,E.equalS,"*********Error*********",
                 errormsg,E.equalS,E.equalS
                ]
      

  --TERMSYNDEF termsynonym -> 
  --    return $ transTermSynonym termsynonym

-- ================================================================================

{-
transOperatorDefn :: OperatorDefn -> M.Defn 
transOperatorDefn x = case x of
  INFIX_LEFT infixrem integer -> 
      M.OperatorDefn (nm,
                      Left $ fromInteger integer,
                      posn 
                     )
          where
            (posn,nm) = transInfixRem infixrem

  INFIX_RIGHT infixrem integer -> 
      M.OperatorDefn (
                      nm,
                      Right $ fromInteger integer,
                      posn
                     )
          where
            (posn,nm) = transInfixRem infixrem


transTermSynonym :: TermSynonym -> M.Defn 
transTermSynonym x = case x of
  TERM_SYNONYM tokterm pident1 infixrem pident2 pident3 pident4 pident5 -> 
      M.TermSyn ( 
                  (snd $ transPIdent pident1,snd $ transInfixRem infixrem,snd $ transPIdent pident2),
                  (snd $ transPIdent pident3,snd $ transPIdent pident4,snd $ transPIdent pident5),
                  transTokTerm tokterm
                )

-}

getDClauseName :: M.DataClause -> M.Name 
getDClauseName (M.DataName (dnm,_),_) = dnm 

insertNamesInList :: (String,[M.Name]) ->
                     State [(String,[M.Name])] (Maybe M.ErrorMsg)
insertNamesInList (ttype,insnmList) = do 
        list <- get 
        case lookup ttype list of 
            Nothing -> do 
                let 
                   newList = (ttype,insnmList):list 
                modify $ \l -> newList
                return Nothing
            Just nmList -> do 
                let 
                  commNms = intersect nmList insnmList
                case commNms == []  of 
                    False  -> do 
                        return $ 
                            Just $ "\nFollowing <<" ++ ttype ++ 
                                   ">> have been defined multiple times.\n" ++
                                   concat (map (\x -> x ++ "\n") commNms) 
                    True -> do 
                        let 
                           oldList = delete (ttype,nmList) list 
                           newList = (ttype,nmList ++ insnmList):oldList
                        modify $ \l -> newList
                        return Nothing    


transTypeDefn :: TypeDefn -> State [(String,[M.Name])] M.Defn 
transTypeDefn x = case x of
  DATA tokdata dataclauses -> do 
       tdclauses <- mapM transDataClause dataclauses
       let 
         dnames = map getDClauseName tdclauses
       dupCheck <- insertNamesInList ("data type",dnames)
       case dupCheck  of 
           Nothing ->  
               return $ M.Data (tdclauses,transTokData tokdata)   
           Just errormsg -> 
               error 
                 $ unlines 
                      [
                       "\n",E.equalS,E.equalS,
                       "*********Error*********",
                       errormsg,E.equalS,E.equalS
                      ]


  CODATA tokcodata codataclauses -> do 
      tdclauses <- mapM transCoDataClause codataclauses
      let 
        cdnames   = map getDClauseName tdclauses
      dupCheck <- insertNamesInList ("codata type",cdnames)
      case dupCheck of 
          Nothing ->
              return $ M.Codata (tdclauses,transTokCodata tokcodata)
          Just errormsg ->
              error $
                unlines
                  [
                    "\n",E.equalS,E.equalS,
                    "*********Error*********",
                    errormsg,E.equalS,E.equalS
                  ]

  TYPE toktype typespecs type_ -> do 
      ttype <- transType type_
      let 
        tSpecs = map (fst.transTypeSpec) typespecs
        tsynms = map (\x -> (x,ttype)) tSpecs 
      return $ M.TypeSyn (tsynms,transTokType toktype) 



-- ==================================================================
-- ==================================================================

transDataClause :: DataClause -> State [(String,[M.Name])] M.DataClause 
transDataClause x = case x of
  DATACLAUSE typespec uident dataphrases -> do 
       let 
         (dposn,datAlt)
                 = transUIdent uident
         datName = M.DataName (transTypeSpec typespec)
         datType = dNametoDType datName dposn        
       datPhrs <- mapM transDataPhrase dataphrases
       let 
          finDphrs = changeDataPhrList (datAlt,datType)
                                       (concat datPhrs)
       return (datName,finDphrs)


{-
here replace the simple string with the dataName
-}
errorMsg :: M.PosnPair -> String
errorMsg (line,col) = "at line,col (" ++ show line ++ ", " ++ show col ++ ")"

changeDataPhrList :: (String,M.Type) ->
                     [(M.Name,[M.Type],String,M.NumArgs,M.PosnPair)] -> 
                     [M.DataPhrase]
changeDataPhrList subst = map (changeDataPhr subst) 

changeDataPhr :: (String,M.Type) -> (M.Name,[M.Type],String,M.NumArgs,M.PosnPair) ->
                 M.DataPhrase
changeDataPhr subst@(str,typeDat) (consName,inTypes,outName,nargs,pn)
        = case outName == str of 
              True  ->
                  (consName,fType,nargs)
                where
                  oType   = typeDat
                  iTypes  = replaceTypes subst inTypes
                  funType = M.TypeFun (iTypes,oType,pn)
                  pVars   = E.getParamVars funType
                  fType   = M.StrFType (pVars,funType)

              False -> do 
                  let 
                    emsg = "Expected <<" ++ str ++ ">> as the output type of <<" ++ 
                            consName ++ " >> but got << " ++ outName ++ ">> " ++ 
                            errorMsg pn 
                  error $ unlines [
                                    "\n",E.equalS,E.equalS,"*********Error*********",
                                    emsg,E.equalS,E.equalS
                                   ]
                   

dNametoDType :: M.DataName -> M.PosnPair -> M.Type 
dNametoDType (M.DataName (dname,args)) dposn
        = M.TypeDataType (dname,E.strToTVar args dposn,dposn)

replaceTypes :: (String,M.Type) -> [M.Type] -> [M.Type]
replaceTypes subst types = map (replaceType subst) types 

replaceType :: (String,M.Type) -> M.Type -> M.Type 
replaceType subst@(str,datType) fType 
    = case fType of 
          M.TypeDataType (name,types,pn) ->
              M.TypeDataType
                  ( name,
                    map (replaceType subst) types,pn
                  )

          M.TypeCodataType (name,types,pn) ->
              M.TypeCodataType 
                  ( name,
                    map (replaceType subst) types,pn
                  )

          M.TypeProd (types,pn) ->
              M.TypeProd (map (replaceType subst) types,pn)

          M.TypeConst (bType,pn) ->
              fType

          M.TypeVar   (var,pn) ->
              case var == str of 
                  True  ->
                      datType
                  False ->
                      fType

          M.TypeFun(types,typeF,pn) ->
              M.TypeFun
                  ( map (replaceType subst) types,
                    replaceType subst typeF,pn
                  )              

          M.Get (type1,type2,pn) ->  
              M.Get 
                  ( replaceType subst type1,
                    replaceType subst type2,pn     
                  )

          M.Put (type1,type2,pn) ->
              M.Put 
                  ( replaceType subst type1,
                    replaceType subst type2,pn     
                  )

          M.Neg (typeN,pn) ->
              M.Neg (replaceType subst typeN,pn)             

          M.ProtNamed (name,types,pn)  -> 
              M.ProtNamed
                  ( name,
                    map (replaceType subst) types,pn
                  )

          M.CoProtNamed (name,types,pn) -> 
              M.CoProtNamed
                  ( name,
                    map (replaceType subst) types,pn
                  )
          M.ProtTensor (type1,type2,pn) ->
              M.ProtTensor 
                  ( replaceType subst type1,
                    replaceType subst type2,pn
                  )

          M.ProtPar (type1,type2,pn) ->
              M.ProtPar 
                  ( replaceType subst type1,
                    replaceType subst type2,pn
                  )

          M.ProtProc (types1,types2,types3,pn) ->
              M.ProtProc
                  ( replaceTypes subst types1,
                    replaceTypes subst types2,
                    replaceTypes subst types3,pn
                  )
          
          otherwise ->
              fType 

-- ================================================================================
-- ================================================================================
-- ================================================================================

dNametoCDType :: M.DataName -> M.PosnPair -> M.Type 
dNametoCDType (M.DataName (dname,args)) dposn
        = M.TypeCodataType (dname,E.strToTVar args dposn,dposn)

transCoDataClause :: CoDataClause -> 
                     State [(String,[M.Name])] M.DataClause

transCoDataClause x = case x of
  CODATACLAUSE uident typespec codataphrases -> do 
       let 
         (dposn,datAlt)
                = transUIdent uident
         cdatName = M.DataName (transTypeSpec typespec)  
         datType  = dNametoCDType cdatName dposn
       cdarPhrs <- mapM transCoDataPhrase codataphrases
       let
         finCDphrs = changeCodataPhrList (datAlt,datType) 
                                         (concat cdarPhrs)
       return (cdatName,finCDphrs)



changeCodataPhrList :: (String,M.Type) -> 
                       [(M.Name,[M.Type],M.Type,M.NumArgs,M.PosnPair)] ->
                       [M.DataPhrase] 
changeCodataPhrList subst  = map (changeCodataPhr subst) 


changeCodataPhr :: (String,M.Type) -> (M.Name,[M.Type],M.Type,M.NumArgs,M.PosnPair) ->
                   M.DataPhrase
changeCodataPhr subst@(str,typeCodat) (destName,inTypes,outType,nargs,pn)
        = case (getStrFromTypeVar (last inTypes) == str) of    
              True  ->
                  (destName,fType,nargs)
                where
                  oType   = replaceType subst outType
                  iTypes  = replaceTypes subst inTypes
                  funType = M.TypeFun (iTypes,oType,pn)
                  pVars   = E.getParamVars funType
                  fType   = M.StrFType (pVars,funType)

              False -> do 
                  let 
                    errormsg =   
                        "Expected <<" ++ str ++ ">> as the destructor name but instead got <<"
                        ++ getStrFromTypeVar (last inTypes) ++ ">>."
                  error $ unlines [
                                    "\n",E.equalS,E.equalS,"*********Error*********",
                                    errormsg,E.equalS,E.equalS
                                   ]


transDataPhrase :: DataPhrase -> 
                   State [(String,[M.Name])] 
                         [(M.Name,[M.Type],String,M.NumArgs,M.PosnPair)]
transDataPhrase x = case x of
  DATAPHRASE structors types uident -> do 
          tTypes <- mapM transType types 
          let 
            nargs    = length tTypes
            tStructs = map transStructor structors 
            consNames= map snd tStructs
            tident   = snd $ transUIdent uident
            qList    = map (\(pn,t) -> (t,tTypes,tident,nargs,pn)) tStructs
          dupCheck <- insertNamesInList ("constructor",consNames)
          case dupCheck of 
              Just errormsg ->
                 error $
                  unlines [
                           "\n",E.equalS,E.equalS,"*********Error*********",
                           errormsg,E.equalS,E.equalS
                          ] 
              Nothing -> 
                  return qList




transCoDataPhrase :: CoDataPhrase -> 
                    State [(String,[M.Name])]
                          [(M.Name,[M.Type],M.Type,M.NumArgs,M.PosnPair)]
transCoDataPhrase x = case x of
  CODATAPHRASE structors types stype -> do 
          tTypes <- mapM transType types 
          tOut   <- transType stype
          let 
            nargs    = length tTypes
            tStructs = map transStructor structors 
            destNames= map snd tStructs
            qList    = map (\(pn,t) -> (t,tTypes,tOut,nargs,pn)) tStructs  
          dupCheck <- insertNamesInList ("destructor",destNames)
          case dupCheck of 
              Just errormsg ->
                 error $
                  unlines [
                           "\n",E.equalS,E.equalS,
                           "*********Error*********",
                           errormsg,E.equalS,E.equalS
                          ] 
              Nothing -> 
                  return qList
            
          
getStrFromTypeVar :: M.Type -> String 
getStrFromTypeVar x  
        = case x of 
              (M.TypeVar (str,posn)) -> 
                  str 
              otherwise -> do 
                  let 
                    errormsg = 
                      "Was expecting a type variable corresponding to the data type "
                      ++ errorMsg (E.getTypePosn x)
                  error $ unlines 
                    [
                      "\n",E.equalS,E.equalS,
                      "*********Error*********",
                      errormsg,E.equalS,E.equalS
                    ]

transStructor :: Structor -> (M.PosnPair,String)
transStructor x = case x of
  STRUCTOR uident -> 
      transUIdent uident

transTypeSpec :: TypeSpec -> (M.Name,[M.Param])
transTypeSpec x = case x of
  TYPESPEC_param uident typeparams -> 
      (
        snd $ transUIdent uident,
        map transTypeParam typeparams
      )

  TYPESPEC_basic uident -> 
      (
        snd $ transUIdent uident,
        []
      )

transTypeParam :: TypeParam -> M.Param
transTypeParam x = case x of
  TYPEPARAM uident -> snd (transUIdent uident)

-- ================================================================================
-- ================================================================================
-- ================================================================================



transType :: Type ->  State [(String,[M.Name])] M.Type 
transType x = case x of
  TYPEARROW typen stype -> do 
       inT  <- transTypeN typen 
       out  <- transType stype
       let 
         cdatTy = M.TypeCodataType ("Exp",[inT,out],E.getTypePosn inT)
       return cdatTy 

  TYPENext typen -> 
      transTypeN typen 

transTypeN :: TypeN -> State [(String,[M.Name])] M.Type 
transTypeN x = case x of
  TYPEUNIT tokunit -> do
      return $ M.Unit (transTokUnit tokunit)  

  TYPELIST _ typen _ -> do
      tTypen <- transTypeN typen 
      return $ M.TypeDataType ("List",[tTypen],E.getTypePosn tTypen) 

  TYPEDATCODAT uident types -> do 
      list <- get 
      let 
        (posn,name) = transUIdent uident
        newList     = transformList list  
      tTypes <- mapM transType types
      case lookup name newList of    
          Just category ->  
              case category == "data type" of 
                  True  -> 
                      return $ M.TypeDataType (name,tTypes,posn)
                  False -> 
                      return $ M.TypeCodataType (name,tTypes,posn)  
          Nothing  -> do 
              let 
                 errormsg =
                      "\nTrying to use a data or codata <<" ++ name ++
                      ">> that hasn't been defined, "
                      ++ errorMsg posn ++ "\n" 
              error $ unlines [
                                "\n",E.equalS,E.equalS,"*********Error*********",
                                errormsg,E.equalS,E.equalS
                              ]


  TYPECONST_VAR uident -> do 
      list <- get 
      let 
        (posn,name)
                = transUIdent uident
        newList = transformList list 
        tType   = fromStrToType (posn,name)
      case lookup name newList of    
          Just category ->  
              case category of 
                 "data type"  ->  
                     return $ M.TypeDataType (name,[],posn)
                 "codata type"-> 
                     return $ M.TypeCodataType (name,[],posn)
                 otherwise   -> 
                     return tType
          
          Nothing ->            
              return tType

  TYPEPROD types -> do 
      cTypes <- mapM transType types 
      let 
        tposn = E.getTypePosn (head cTypes)
      return $ M.TypeProd (cTypes,tposn) 

  TYPEBRACKET stype -> do 
      transType stype



fromStrToType :: (M.PosnPair,String) -> M.Type 
fromStrToType (posn,str) 
        | str == "Int"    = M.TypeConst (M.BaseInt,posn)
        | str == "Double" = M.TypeConst (M.BaseDouble,posn)
        | str == "Char"   = M.TypeConst (M.BaseChar,posn)
        | str == "String" =  M.TypeConst (M.BaseString,posn)
        | otherwise       = M.TypeVar (str,posn)
       

transformList :: [(String,[M.Name])] -> [(M.Name,String)]
transformList [] 
        = []
transformList ((str,names):rest) 
        = (map (\x -> (x,str)) names) ++
          (transformList rest)     

-- ================================================================================
-- ================================================================================
-- ================================================================================

transCTypeDefn :: CTypeDefn -> State [(String,[M.Name])] M.Defn
transCTypeDefn x = case x of
  PROTOCOL tokprotocol protocolclause -> do 
      pclause  <- transProtocolClause protocolclause
      dupCheck <- insertNamesInList 
                      ("Protocol",
                        [getNameFromPClause pclause]
                      )
      case dupCheck  of 
          Nothing ->  
              return $ M.ProtocolDefn 
                  ( [pclause],
                    transTokProtocol tokprotocol
                  )   
          Just errormsg -> 
              error $ unlines 
                [
                  "\n",E.equalS,E.equalS,"*********Error*********",
                  errormsg,E.equalS,E.equalS
                ]

  COPROTOCOL tokcoprotocol coprotocolclause -> do 
      pclause <- transCoProtocolClause coprotocolclause
      dupCheck <- insertNamesInList 
                     ("Coprotocol",
                      [getNameFromPClause pclause]
                     )
      case dupCheck  of 
          Nothing ->  
              return $ M.CoprotocolDefn 
                  ( [pclause],
                    transTokCoprotocol tokcoprotocol
                  )   
          Just errormsg -> 
              error $ unlines 
                [
                  "\n",E.equalS,E.equalS,"*********Error*********",
                  errormsg,E.equalS,E.equalS
                ]

getNameFromPClause :: M.ProtocolClause -> M.Name 
getNameFromPClause (M.DataName (dname,_),_)
        = dname 
-- ============================================================
-- ============================================================
dNametoProtocol:: M.DataName -> M.PosnPair -> M.Type 
dNametoProtocol (M.DataName (dname,args)) dposn
        = M.ProtNamed (dname,E.strToTVar args dposn,dposn)


transProtocolClause :: ProtocolClause ->
                       State [(String,[M.Name])] M.ProtocolClause
transProtocolClause x = case x of
  PROTOCOLCLAUSE typespec uident protocolphrases -> do 
      let 
         (pPosn,pAlt) 
                 = transUIdent uident 
         datName = M.DataName (transTypeSpec typespec)
         protocol= dNametoProtocol datName pPosn
         subst   = (pAlt,protocol)
      protPhrs <- mapM (transProtocolPhrase subst)
                       protocolphrases  
      --let 
        --finPPhrs = changeProtPhrList (pAlt,protocol)                 
      return (datName,protPhrs)

-- ============================================================
-- ============================================================

dNametoCoProtocol:: M.DataName -> M.PosnPair -> M.Type 
dNametoCoProtocol (M.DataName (dname,args)) dposn
        = M.CoProtNamed (dname,E.strToTVar args dposn,dposn)

transCoProtocolClause :: CoProtocolClause -> 
                         State [(String,[M.Name])] M.ProtocolClause
transCoProtocolClause x = case x of
  COPROTOCOLCLAUSE uident typespec coprotocolphrases -> do 
      let 
         datName = M.DataName (transTypeSpec typespec)
         (protPosn,protVar) 
                 = transUIdent uident
         coprot  = dNametoCoProtocol datName protPosn
         subst   = (protVar,coprot)
      cprotPhrs <- mapM (transCoProtocolPhrase subst) 
                        coprotocolphrases   
      return (datName,cprotPhrs)

-- ============================================================
-- ============================================================

transProtocolPhrase :: (M.Name,M.Type) -> ProtocolPhrase -> 
                       State [(String,[M.Name])] (M.Name,M.FunType)

transProtocolPhrase subsProt@(var,prot) x = case x of
  PROTOCOLPHRASE uident1 protocol uident2 -> do 
      let 
        handName      = snd $ transUIdent uident1 
        (posn,handVar)= transUIdent uident2
      case var == handVar of 
          False ->
              error $
                "Unknown variable <<" ++ handVar ++ ">>" ++
                E.printPosn posn ++ "\n. Expecting variable <<" 
                ++ var ++ ">> instead."

          True  -> do 
              dupCheck <- insertNamesInList ("handle",[handName])
              case dupCheck of 
                  Just errormsg ->
                     error $
                        unlines [
                                 "\n",E.equalS,E.equalS,
                                 "*********Error*********",
                                 errormsg,E.equalS,E.equalS
                                ] 
                  Nothing -> do
                      tProt <- transProtocol protocol
                      let
                        substProt = replaceType subsProt tProt  
                        funType = M.TypeFun ([substProt],prot,posn)
                        pVars   = E.getParamVars funType
                        fType   = M.StrFType (pVars,funType)
                      return (handName,fType) 




transCoProtocolPhrase :: (M.Name,M.Type) -> CoProtocolPhrase ->
                         State [(String,[M.Name])] (M.Name,M.FunType)

transCoProtocolPhrase subsProt@(var,coProt) x = case x of
  COPROTOCOLPHRASE uident1 uident2 protocol -> do 
      let 
        chandName   = snd $ transUIdent uident1
        (cposn,cvar)= transUIdent uident2
      case var == cvar of 
          False ->
              error $
                "Unknown variable <<" ++ cvar ++">>" ++
                E.printPosn cposn ++ "\n. Expecting variable <<" 
                ++ var ++ ">> instead."

          True  -> do 
              dupCheck <- insertNamesInList ("cohandle",[chandName])
              case dupCheck of 
                  Just errormsg ->
                     error $
                        unlines 
                          [
                            "\n",E.equalS,E.equalS,
                            "*********Error*********",
                            errormsg,E.equalS,E.equalS
                          ] 
                  Nothing -> do
                      tProt <- transProtocol protocol 
                      let
                        substProt = replaceType subsProt tProt  
                        funType = M.TypeFun ([substProt],coProt,cposn)
                        pVars   = E.getParamVars funType
                        fType   = M.StrFType (pVars,funType)
                      return (chandName,fType) 


-- ======================================================
-- ======================================================

transProtocol :: Protocol -> 
                 State [(String,[M.Name])] M.Type 

transProtocol x = case x of
  PROTOCOLtensor protocol1 protocol2 -> do 
      tProt1 <- transProtocol protocol1
      tProt2 <- transProtocol protocol2 
      return $ 
          M.ProtTensor 
            (tProt1,tProt2,E.getTypePosn tProt1)
            

  PROTOCOLpar protocol1 protocol2 -> do 
      tProt1 <- transProtocol protocol1
      tProt2 <- transProtocol protocol2 
      return $ 
          M.ProtPar 
              (tProt1,tProt2,E.getTypePosn tProt1) 

  PROTOCOLget tokgetprot type_ protocol -> do 
      tProt1 <- transProtocol protocol
      tType  <- transType type_ 
      let 
        gPosn = transTokGetProt tokgetprot
      return $
          M.Get (tType,tProt1,gPosn)   

  PROTOCOLput tokputprot type_ protocol -> do 
      tProt1 <- transProtocol protocol
      tType  <- transType type_ 
      let 
        gPosn = transTokPutProt tokputprot
      return $ 
         M.Put (tType,tProt1,gPosn) 

  PROTOCOLneg protocol ->  do 
      tProt1 <- transProtocol protocol 
      return $ 
          M.Neg (tProt1,E.getTypePosn tProt1)

  PROTOCOLtopbot toktopbot -> do 
      return $ 
          M.TopBot (transTokTopBot toktopbot)

  PROTNamedWArgs uident types -> do 
      list <- get
      tTypes <- mapM transType types
      let 
         (tPosn,name)
                 = transUIdent uident 
         newList = transformList list 
      case lookup name newList of 
          Just category ->
              case category == "Protocol" of 
                  True  -> 
                      return $ M.ProtNamed (name,tTypes,tPosn)
                  False ->
                      return $ M.CoProtNamed (name,tTypes,tPosn)
                          

          Nothing -> do      
              let 
                 errormsg =
                   "\nTrying to use a Protocol/Coprotocol <<" 
                   ++ name ++ ">> that hasn't been defined, "
                   ++ errorMsg tPosn ++ show list  
              error $ unlines
                 [
                   "\n",E.equalS,E.equalS,"*********Error*********",
                   errormsg,E.equalS,E.equalS
                 ] 


  PROTNamedWOArgs uident -> do 
      list <- get
      let 
         (tPosn,name)  = transUIdent uident 
         newList       = transformList list 
         tType         = fromStrToType_Prot (tPosn,name)
      case lookup name newList of 
          Just category ->
              case category  of 
                  "Protocol"  -> 
                      return $ M.ProtNamed (name,[],tPosn)
                  "Coprotocol" -> 
                      return $ M.CoProtNamed(name,[],tPosn)
                  otherwise ->
                      return tType

          Nothing -> do      
              return tType

fromStrToType_Prot :: (M.PosnPair,String) -> M.Type 
fromStrToType_Prot (posn,str) 
        | elem str constList
              = error $ unlines
                  [
                    "\n",E.equalS,E.equalS,"*********Error*********",
                    errormsg,E.equalS,E.equalS
                  ]

        | otherwise  
              = M.TypeVar (str,posn)
    where
       constList = ["Int","String","Char","Double"]
       errormsg  = "Expecting a protocol variable and not a <<"
                    ++ str ++ ">>." 




getFunctionDefnName :: FunctionDefn -> [M.Name]
getFunctionDefnName fdefn 
        = case fdefn of 
              FUNCTIONDEFNfull _ pident _ _ _ ->
                  [snd $ transPIdent pident]
              FUNCTIONDEFNshort _ pident _ ->
                  [snd $ transPIdent pident]
    
getProcDefnName :: ProcessDef -> [M.Name]
getProcDefnName pdef 
        = case pdef of 
              PROCESSDEFfull _ pid _ _ _ _->
                  [snd $ transPIdent pid]
              PROCESSDEFshort _ pid _  -> 
                  [snd $ transPIdent pid] 


transFunctionDefn :: FunctionDefn -> State [(String,[M.Name])] M.Defn
transFunctionDefn x = case x of
  FUNCTIONDEFNfull tokfun pident types type_ patttermpharses -> do 
          let 
             fposn  = transTokFun tokfun 
             fname  = M.Custom $ snd $ transPIdent pident
          iTypes <- mapM transType types 
          oType  <- transType type_
          tPattPhs <- mapM transPattTermPharse patttermpharses 
          let 
             funType = M.TypeFun (iTypes,oType,fposn)
             parVars = E.getParamVars funType
             fType   = M.StrFType (parVars,funType)
             funDefn = M.FunctionDefn (fname,fType,tPattPhs,fposn)
          return funDefn

  FUNCTIONDEFNshort tokfun pident patttermpharses -> do 
          tPattPhs <- mapM transPattTermPharse patttermpharses 
          let 
             fposn   = transTokFun tokfun
             fname   = M.Custom $ snd $ transPIdent pident 
             funDefn = M.FunctionDefn (fname,M.NoType,tPattPhs,fposn)
          return funDefn


transFoldPattern :: FoldPattern -> State [(String,[M.Name])] M.FoldPattern
transFoldPattern x = case x of
  FOLDPATT_WOBRAC uident pidents term -> do 
       let 
         (posn,name) = transUIdent uident
         vPatts      = map ((M.VarPattern).swap.transPIdent) pidents
       tTerm <- transTerm term         
       return $ (name,vPatts,tTerm,posn)  


swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

transPattTermPharse :: PattTermPharse -> 
                       State [(String,[M.Name])] (M.PatternTermPhr,M.PosnPair)
transPattTermPharse x = case x of
  PATTERNshort patterns term -> do 
        tTerm  <- transTerm term 
        tPatts <- mapM transPattern patterns
        case tPatts /= [] of 
          True -> do 
            let
              tPosn  = E.getPattPosn (head tPatts)
            return $ (
                       (tPatts,Left tTerm),
                       tPosn
                     ) 

          False ->  do 
            let
              tPosn  = E.getTermPosn tTerm
            return $ (
                       ([M.NoPattern tPosn],Left tTerm),
                       tPosn
                     ) 
      
  PATTERNguard patterns guardedterms -> do 
        tTerms <- mapM transGuardedTerm guardedterms
        tPatts <- mapM transPattern patterns
        let 
          tPosn  = E.getPattPosn (head tPatts)
        return ((tPatts,Right tTerms),tPosn)

{-
Throw an error if there are no default guards 
if there is only default guard get back a term instead of a guard.
switch
   | default = term is just term 

otherise return the normal guarded term
-}
checkGuards :: [M.GuardedTerm] -> 
               Either M.ErrorMsg (Either M.Term [M.GuardedTerm])
checkGuards guards = do 
    let 
      (termL,termR) = (last guards) 
      remGuards     = init guards   
    case termL of 
      M.TDefault _ -> 
        case remGuards of 
          [] -> do
            return (Left termR) 

          otherwise -> do 
            return (Right guards)

      otherwise  -> do 
        let 
          emsg
            = "Last guard needs to be a default" ++
               E.printPosn (E.getTermPosn termL)

        error $ unlines
            [
              "\n",E.equalS,E.equalS,"*********Error*********",
              emsg,E.equalS,E.equalS
            ] 



transGuardedTerm :: GuardedTerm -> State [(String,[M.Name])] M.GuardedTerm
transGuardedTerm x = case x of
  GUARDterm term1 term2 -> do 
          tTerm1 <- transTerm term1 
          tTerm2 <- transTerm term2  
          return (tTerm1,tTerm2)

  GUARDother tokdefault term -> do
          tTerm <- transTerm term  
          return (
                  M.TDefault (transTokDefault tokdefault),
                  tTerm
                 )

-- ============================================================
-- ============================================================
-- ============================================================

                

transPattern :: Pattern -> 
                State [(String,[M.Name])] M.Pattern
transPattern x = case x of
  LISTPATTERN2 pattern1 pattern2 -> do 
          tPatt1 <- transPattern pattern1
          tPatt2 <- transPattern pattern2
          let 
            tPosn  = E.getPattPosn tPatt1 
          return $ M.ConsPattern ("Cons",[tPatt1,tPatt2],tPosn)
                
  CONSPATTERN uident patterns -> do 
         list <- get 
         let 
           (tPosn,name)
                   = transUIdent uident
           newList = transformList list 
         tPatts <- mapM transPattern patterns 
         case lookup name newList of    
             Just category ->  
                 case category == "constructor" of 
                     True  ->
                         return $ 
                              M.ConsPattern 
                                  (name,tPatts,tPosn) 
                     False ->
                         return $ 
                              M.DestPattern
                                  (name,tPatts,tPosn)  

             Nothing -> do      
                 let 
                    errormsg =
                        "\nTrying to use a constructor/destructor <<" 
                        ++ name ++ ">> that hasn't been defined, "
                        ++ errorMsg tPosn 
                 error 
                    $ unlines
                          [
                            "\n",E.equalS,E.equalS,
                            "*********Error*********",
                            errormsg,E.equalS,E.equalS
                          ] 

  CONSPATTERN_WA uident -> do 
         list <- get 
         let 
           (tPosn,name)
                   = transUIdent uident
           newList = transformList list 
         case lookup name newList of    
             Just category ->  
                 case category == "constructor" of 
                     True  ->
                         return $ 
                             M.ConsPattern
                                (name,[],tPosn) 
                     False ->
                         return $
                             M.DestPattern
                                (name,[],tPosn)  

             Nothing -> do      
                 let 
                     errormsg =
                          "\nTrying to use a constructor/destructor <<" ++ name ++  
                          ">> that hasn't been defined, "
                          ++ errorMsg tPosn 
                 error $ 
                    unlines [
                              "\n",E.equalS,E.equalS,
                              "*********Error*********",
                              errormsg,E.equalS,E.equalS
                            ] 

  LISTPATTERN1 ob patterns cb -> 
      case length patterns == 0 of 
          True  ->
              return $ 
                  M.ConsPattern
                    ("Nil",[],transTokSBrO ob)

          False -> do 
              tPatt     <- transPattern (head patterns)
              restTPatt <- transPattern 
                             (LISTPATTERN1 ob (tail patterns) cb) 
              return $ M.ConsPattern 
                         (
                          "Cons",
                          [tPatt,restTPatt],
                          E.getPattPosn tPatt
                         )
            
        
  PRODPATTERN patterns -> 
      case length patterns == 0 of 
          True  -> 
            error $ unlines
              [
               "\n",E.equalS,E.equalS,"*********Error*********",
               "Empty Tuple in pattern",E.equalS,E.equalS
              ]

          False -> 
              case length patterns == 1 of 
                  True  -> 
                      transPattern (head patterns)
                  False -> do 
                      tPatts <- mapM transPattern patterns
                      let 
                        tPosn = E.getPattPosn (head tPatts)
                      return $ M.ProdPattern (tPatts,tPosn) 

  VARPATTERN pident -> 
         return $ M.VarPattern (tName,tPosn)
      where
        (tPosn,tName) = transPIdent pident  

  STR_CONSTPATTERN str ->
         return $ M.StrConstPattern (str,(0,0))

  INT_CONSTPATTERN pinteger -> 
         return $ M.IntConstPattern (num,tPosn)
      where
        (tPosn,num) = transPInteger pinteger 

  NULLPATTERN nullPatt -> 
         return $ M.DontCarePattern (transTokDCare nullPatt)

-- ==========================================================================
-- ==========================================================================
-- ==========================================================================

transTerm :: Term -> State [(String,[M.Name])] M.Term 
transTerm x = case x of
  RECORDTERM tokrecord recordentrys -> do  
      tRecs <- mapM transRecordEntry recordentrys
      return $ M.TRecord tRecs

  RECORDTERMALT recordentryalts -> do 
      tRecs <- mapM transRecordEntryAlt recordentryalts 
      return $ M.TRecord tRecs

  --InfixTerm term1 infixrem term2 -> 
  --        undefined 

  Infix0TERM term3 i0op term4 -> do 
      tTerm3 <- transTerm term3 
      tTerm4 <- transTerm term4 
      return $ M.TCallFun 
                  (
                     transInfix0op i0op,
                     [tTerm3,tTerm4],
                     E.getTermPosn tTerm3
                  )               

  Infix1TERM term4 i1op term5 -> do 
      tTerm4 <- transTerm term4 
      tTerm5 <- transTerm term5 
      return $ M.TCallFun 
                  (
                    transInfix1op i1op,
                    [tTerm4,tTerm5],
                    E.getTermPosn tTerm4
                  ) 

  Infix2TERM term5 i2op term6 -> do 
      tTerm5  <- transTerm term5 
      tTerm6  <- transTerm term6 
      return $ M.TCallFun 
                  (
                    transInfix2op i2op,
                    [tTerm5,tTerm6],
                    E.getTermPosn tTerm5
                  ) 

  Infix3TERM term6 i3op term7 -> do 
      tTerm6  <- transTerm term6 
      tTerm7  <- transTerm term7 
      return $ M.TCallFun 
                  (
                    transInfix3op i3op,
                    [tTerm6,tTerm7],
                    E.getTermPosn tTerm6
                  ) 

  Infix4TERM term7 i4op term8 -> do 
      tTerm6  <- transTerm term7 
      tTerm7  <- transTerm term8 
      return $ M.TCallFun 
                  (
                    transInfix4op i4op,
                    [tTerm6,tTerm7],
                    E.getTermPosn tTerm7
                  ) 


  Infix5TERM term8 i5op term9 -> do 
      tTerm8  <- transTerm term8 
      tTerm9  <- transTerm term9 
      return $ M.TCallFun 
                  (
                    transInfix5op i5op,
                    [tTerm8,tTerm9],
                    E.getTermPosn tTerm8
                  ) 


  Infix6TERM term10 i6op term9 -> do 
      tTerm10  <- transTerm term10 
      tTerm9   <- transTerm term9 
      return $ M.TCallFun 
                  (
                    transInfix6op i6op,
                    [tTerm10,tTerm9],
                    E.getTermPosn tTerm9
                  ) 

  Infix7TERM term10 i7op term11 -> do 
      tTerm10  <- transTerm term10 
      tTerm11   <- transTerm term11 
      return $ M.TCallFun 
                  (
                    transInfix7op i7op,
                    [tTerm10,tTerm11],
                    E.getTermPosn tTerm10
                  ) 


  LISTTERM2 term1 term2 -> do 
      tTerm1 <- transTerm term1 
      tTerm2 <- transTerm term2 
      return $ M.TCons (
                          "Cons",[tTerm1,tTerm2], 
                          E.getTermPosn tTerm1
                        )

  LISTTERM ob terms cb -> 
      case length terms == 0 of 
          True  ->
              return $ M.TCons ("Nil",[],transTokSBrO ob)
          False -> do 
              tTerm <- transTerm (head terms) 
              tRest <- transTerm $ LISTTERM ob (tail terms) cb
              let 
                tPosn  = E.getTermPosn tTerm
              return $ M.TCons ("Cons",[tTerm,tRest],tPosn)
         
  LETTERM term lwheres -> do 
      tletwhrs <- mapM transLetWhere lwheres 
      tTerm    <- transTerm term
      return $ M.TLet (tTerm,tletwhrs,E.getTermPosn tTerm)

  VARTERM pident -> do 
      list <- get 
      let 
        (vPosn,vName) = transPIdent pident 
      case lookup "function" list of 
         Nothing ->
           return $ M.TVar (vName,vPosn)
         Just flist -> do 
           case elem vName flist of 
             True  ->
                return $ M.TCallFun (detectFun vName,[],vPosn)
             False ->
                return $ M.TVar (vName,vPosn)  

  CONSTTERM constanttype -> do            
      return $ M.TConst (transConstantType constanttype)

  IFTERM tokif term1 term2 term3 -> do 
      tTerm1 <- transTerm term1
      tTerm2 <- transTerm term2
      tTerm3 <- transTerm term3 
      return $ M.TIf (tTerm1,tTerm2,tTerm3,transTokIf tokif)

  UNFOLDTERM tokunfold pident foldpatterns -> 
          undefined 

  FOLDTERM tokfold pident foldpatterns -> do 
      tFoldPatts <- mapM transFoldPattern foldpatterns
      return $ M.TFold (
                        ((M.TVar).swap.transPIdent) pident,
                        tFoldPatts,
                        transTokFold tokfold
                      )

  CASETERM tokcase term patttermpharses -> do 
      tTerm <- transTerm term
      tPattTerms <- mapM transPattTermPharse patttermpharses 
      return $ M.TCase ( 
                        tTerm,
                        map fst tPattTerms,
                        transTokCase tokcase 
                       )

  GENCONSTERM_WARGS uident terms -> do
      list <- get        
      let 
        (posn,name) 
                = transUIdent uident
        newList = transformList list  
      tTerms <- mapM transTerm terms  
      case lookup name newList of  
        Just category ->  
            case category == "constructor" of 
                True  -> 
                    return $ M.TCons (name,tTerms,posn)
                False -> 
                    return $ M.TDest (name,tTerms,posn)

        Nothing  -> do 
            let 
              errormsg =
                "\nTrying to use a constructor/destructor <<" 
                ++ name ++">> that hasn't been defined, " ++
                errorMsg posn 
            error $ unlines
              [
               "\n",E.equalS,E.equalS,"*********Error*********",
               errormsg,E.equalS,E.equalS
              ]            
        
  GENCONSTERM_WOARGS uident -> do 
      list <- get        
      let 
        (posn,name) 
                = transUIdent uident
        newList = transformList list  
      case lookup name newList of  
        Just category ->  
            case category == "constructor" of 
                True  -> 
                    return $ M.TCons (name,[],posn)
                False -> 
                    return $ M.TDest (name,[],posn)

        Nothing  -> do 
            let 
              errormsg =
                "\nTrying to use a constructor/destructor <<" 
                ++ name ++ ">> that hasn't been defined, "++ 
                errorMsg posn 
            error $ unlines
              [
                "\n",E.equalS,E.equalS,"*********Error*********",
                errormsg,E.equalS,E.equalS
              ]
                                
  PRODTERM terms -> do 
          tTerms <- mapM transTerm terms
          let 
            tPosn  = E.getTermPosn (head tTerms)   
          return $ M.TProd (tTerms,tPosn)     

  FUNAPPTERM pident terms -> do 
        tTerms <- mapM transTerm terms 
        let 
          (posn,name)
               = transPIdent pident
        return $ M.TCallFun (detectFun name,tTerms,posn)


transLetWhere :: LetWhere -> State [(String,[M.Name])] M.LetWhere
transLetWhere lwhs
   = case lwhs of 
         DEFN_LetWhere defn -> do 
             tDefn <- transDefn defn
             return $ M.LetDefn tDefn
         PATTTERM_LetWhere pattTerm -> do
             tPattTerm <- transPattTerm pattTerm 
             return $ M.LetPatt tPattTerm

transPattTerm :: PattTerm -> 
                 State [(String,[M.Name])] (M.Pattern,M.Term)
transPattTerm (JUSTPATTTERM pident term) = do 
         let 
           (strpn,str) = transPIdent pident
           varPatt     = M.VarPattern (str,strpn)
         tTerm <- transTerm term 
         return (varPatt,tTerm)  


detectFun :: M.Name -> M.FuncName
detectFun name 
        | name == "addI"  = M.BuiltIn (M.Add_I)
        | name == "subI"  = M.BuiltIn (M.Sub_I)
        | name == "mulI"  = M.BuiltIn (M.Mul_I)
        | name == "quotI" = M.BuiltIn (M.DivQ_I)
        | name == "remI"  = M.BuiltIn (M.DivR_I) 
        | name == "eqI"   = M.BuiltIn (M.Eq_I)
        | name == "leqI"  = M.BuiltIn (M.Leq_I)
        | name == "notEqI"= M.BuiltIn (M.Neq_I)
        | name == "lessI" = M.BuiltIn (M.LT_I)
        | name == "grtI"  = M.BuiltIn (M.GT_I) 
        | name == "geqI"  = M.BuiltIn (M.Geq_I)
        | name == "eqC"   = M.BuiltIn (M.Eq_C)
        | name == "eqS"   = M.BuiltIn (M.Eq_S)
        | name == "concat" = M.BuiltIn (M.Concat_S)
        | name == "appendL"= M.BuiltIn (M.Append)
        | name == "unstring"   = M.BuiltIn (M.Unstring_S)
        | name == "toStr" = M.BuiltIn (M.ToStr)
        | name == "toInt" = M.BuiltIn (M.ToInt)
        | name == "orB"         = M.BuiltIn (M.Or_B)
        | name == "andB"        = M.BuiltIn (M.And_B)
        | otherwise            = M.Custom name


-- ==========================================================================
-- ==========================================================================
-- ==========================================================================

transConstantType :: ConstantType -> (M.BaseVal,M.PosnPair)
transConstantType x = case x of
  INTEGER pinteger -> 
        (M.ConstInt num,posn)
      where
       (posn,num) = transPInteger pinteger

  STRING string -> 
        (M.ConstString string,(0,0)) 

  DOUBLE double -> 
        (M.ConstDouble double,(0,0))

  CHAR char ->
        (M.ConstChar char,(0,0))

transRecordEntryAlt :: RecordEntryAlt -> 
                       State [(String,[M.Name])] 
                          (M.Pattern,M.Term,M.PosnPair)
transRecordEntryAlt x = case x of
  RECORDENTRY_ALT recEntry -> do 
        transRecordEntry recEntry 


transRecordEntry :: RecordEntry ->
                    State [(String,[M.Name])] 
                        (M.Pattern,M.Term,M.PosnPair)

transRecordEntry x = case x of
  RECORDENTRY patt term -> do
        destPatt <- transPattern patt
        tTerm    <- transTerm term 
        return (destPatt,tTerm,E.getPattPosn destPatt) 



-- =============================================================================
-- =============================================================================
-- =============================================================================

transProcessDef :: ProcessDef -> State [(String,[M.Name])] M.Defn 
transProcessDef x = case x of
  PROCESSDEFfull tokproc pident types protocols1 
                 protocols2 patprocessphr -> do 
      tTypes  <- mapM transType types 
      tProts1 <- mapM transProtocol protocols1 
      tProts2 <- mapM transProtocol protocols2
      let 
        procType = M.ProtProc
                    (
                      tTypes,tProts1,tProts2,
                      transTokProc tokproc
                    )
        pnmPosn = transPIdent pident 
        pVars   = E.getParamVars procType
        fType   = M.StrFType (pVars,procType)       

      newPtPhr <- transPatProcessPhr pnmPosn patprocessphr 

      return $ 
        M.ProcessDefn 
            ( 
             snd pnmPosn ,fType,newPtPhr,
             transTokProc tokproc 
            )

  PROCESSDEFshort tokproc pident patprocessphr -> do 
      let 
        pnmPosn = transPIdent pident
      newPtPhr <- transPatProcessPhr pnmPosn patprocessphr            
      return $ 
          M.ProcessDefn 
              ( 
               snd pnmPosn,M.NoType,newPtPhr,
               transTokProc tokproc 
              )

transPatProcessPhr :: (M.PosnPair,M.Name) ->  PatProcessPhr ->
                      State [(String,[M.Name])] M.PattProcessPhr

transPatProcessPhr (pPosn,pName) x = case x of            
  PROCESSPHRASEnoguard patterns channels1 channels2 process -> do  
      tPatts  <- mapM transPattern patterns
      let 
        tChs1 = map transChannel channels1
        tChs2 = map transChannel channels2
      tProc <- transProcess process
      let 
        obchs    = getChs_proc tProc 
        remChs   = (S.\\) (S.fromList (tChs1 ++ tChs2)) 
                          (S.fromList obchs)
      case null remChs of 
          True ->     
              return (tPatts,tChs1,tChs2,tProc)  
          False -> do
              let
                emsg 
                  = "In Process <<" ++ pName ++ ">>" ++ E.printPosn pPosn  ++ 
                    ", these channels haven't been closed\n<<" ++
                    intercalate "," (S.toList remChs) ++ ">>"

              error $ unlines
                  [ "\n",E.equalS,E.equalS,"*********Error*********","\n",
                    emsg,E.equalS,E.equalS 
                  ]                  

                              

transProcess :: Process -> 
               State [(String,[M.Name])] M.Process
transProcess x = case x of
  MANY_PROCESS processcommands -> do 
      pcomms <- mapM transProcessCommand processcommands
      return pcomms

  ONE_PROCESS processcommand -> do 
      pcomm <- transProcessCommand processcommand 
      return [pcomm]


transProcessCommand :: ProcessCommand -> 
                       State [(String,[M.Name])] M.ProcessCommand
transProcessCommand x = case x of
  PROCESS_RUN pident terms channels1 channels2 -> do 
      tTerms <- mapM transTerm terms
      let 
        nmPair = transPIdent pident
      return $
          M.PRun 
            (
              snd nmPair,
              tTerms,
              map transChannel channels1,
              map transChannel channels2,
              fst nmPair
            )

  PROCESS_CLOSE tokclose channel -> 
      return $ 
          M.PClose
             (
               transChannel channel,
               transTokClose tokclose
              )  

  PROCESS_HALT tokhalt channel -> 
      return $ 
          M.PHalt
            (
               transChannel channel,
               transTokHalt tokhalt  
             )

  PROCESS_GET tokget pident channel -> do  
      let 
        (posn,var)= transPIdent pident 
        varPatt   = M.VarPattern (var,posn)
      return $  
         M.PGet ( varPatt,
                  transChannel channel,
                  transTokGet tokget
                )

  PROCESS_HCASE tokhcase channel handlers -> do 
      tHands <- mapM transHandler handlers 
      return $ M.PHCase 
          ( 
            transChannel channel,
            tHands,
            transTokHCase tokhcase
          )  

  PROCESS_PUT tokput term channel -> do 
      tTerm <- transTerm term
      return $ M.PPut 
          (
           tTerm,
           transChannel channel,
           transTokPut tokput
          )

  PROCESS_HPUT tokhput uident channel -> do 
      return $ M.PHPut
          (
           snd $ transUIdent uident,  
           transChannel channel,
           transTokHPut tokhput
          )

  PROCESS_SPLIT toksplit channel channels -> do 
      let 
        tchs = map transChannel channels
      return $ M.PSplit 
          (
            transChannel channel,
            (tchs!!0,tchs !! 1),
            transTokSplit toksplit 
          )

  PROCESS_FORK tokfork pident forkparts -> do 
      tforkParts <- mapM transForkPart forkparts
      return $ M.PFork 
          (
            snd $ transPIdent pident,
            tforkParts,
            transTokFork tokfork
          )    

  Process_PLUG plugparts -> do 
      procs <- mapM transPlugPart plugparts 
      let 
        posn = (E.getProcPosn.head.head) procs 
        pcom = convertToPlug procs posn 
      case pcom of 
        Left emsg -> 
          error emsg 
        Right plugCom -> 
          return plugCom   
      
  Procss_ID channel pchannel -> do 
      return $ M.PId 
          (
            transChannel channel,
            transPChannel pchannel,
            getPosnChannel channel
          )

  PROCESScase tokcase term processphrases -> do 
      tTerm  <- transTerm term
      pProcs <- mapM transProcessPhrase processphrases
      return $ M.PCase 
          (tTerm,pProcs,transTokCase tokcase) 

  PROCESS_NEG ch1 ch2 -> do 
      return $
          M.PNeg
            (
               transChannel ch1,
               transChannel ch2,
               getPosnChannel ch1 
             ) 

-- ===============================================================
-- ===============================================================
-- ===============================================================

transPlugPart :: PlugPart -> 
                 State [(String,[M.Name])] M.Process 
transPlugPart x = case x of
  PLUGPART_MANY processcommands -> do
      mapM transProcessCommand processcommands 
        

  PLUGPART_ONE processcommand -> do 
      tProc <- transProcessCommand processcommand
      return [tProc]

-- =================================================
-- ================================================= 
 

transForkPart :: ForkPart -> 
                 State [(String,[M.Name])] (M.PChannel,[M.PChannel], M.Process)
transForkPart x = case x of
  FORKPARTshort pident process -> do 
      tProcs <- transProcess process 
      let 
        chs = getChs_proc tProcs
      return (snd $ transPIdent pident,chs,tProcs)     


transHandler :: Handler -> 
                State [(String,[M.Name])] 
                (M.Name,M.Process,M.PosnPair)
transHandler x = case x of
  HANDLER uident process -> do 
    let 
      (posn,name) = transUIdent uident
    tProcs <- transProcess process 
    return (name,tProcs,posn)

-- ==========================================================
-- ==========================================================

transProcessPhrase :: ProcessPhrase -> 
                      State [(String,[M.Name])] M.PattProc

transProcessPhrase x = case x of       
  CASEPROCESSnoguard pattern process -> do 
      tPatt <- transPattern pattern
      tProc <- transProcess process 
      return (tPatt,tProc)

-- ==========================================================
-- ==========================================================

transChannel :: Channel -> M.PChannel
transChannel x = case x of
  CHANNEL pident -> snd $ transPIdent pident

transPChannel :: PChannel -> M.Channel
transPChannel x = case x of 
  BARECHANNEL pident -> M.PosChan
                          (snd $ transPIdent pident)
  NEGCHANNEL  pident -> M.NegChan
                          (snd $ transPIdent pident)  


getPosnChannel :: Channel -> M.PosnPair
getPosnChannel x = case x of
  CHANNEL pident -> fst $ transPIdent pident

-- ==========================================================
-- ==========================================================