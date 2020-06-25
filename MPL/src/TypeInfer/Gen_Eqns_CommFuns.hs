module TypeInfer.Gen_Eqns_CommFuns where

import TypeInfer.SymTab_DataType
import TypeInfer.MPL_AST

import Control.Monad.State
import Control.Monad.Except
-- import Control.Monad.Trans.Either
import Data.List 

equalS :: String
equalS = replicate 80 '='

stars :: String
stars  = replicate 80 '*'

getParamVars :: Type -> [String]
getParamVars typeP = nub (getTypeVars typeP)

getTypeVars :: Type -> [String]
getTypeVars sType 
    = case sType of 
        Unit pn -> 
            []
        
        TypeDataType (_,types,pn) ->
            concat $ map getTypeVars types 

        TypeCodataType (_,types,pn) ->
            concat $ map getTypeVars types 

        TypeProd (types,pn) ->
            concat $ map getTypeVars types 

        TypeConst _ ->
            [] 

        TypeVar (t,pn) -> 
            [t]

        TypeVarInt _ ->
            []

        TypeFun (types,stype,pn) ->
            concat $ map getTypeVars (stype:types)

        Get (type1,type2,_) ->  
            getTypeVars type1 ++
            getTypeVars type2 

        Put (type1,type2,_) ->
            getTypeVars type1 ++
            getTypeVars type2 

        Neg (typeN,_) ->
            getTypeVars typeN 

        TopBot _ ->
            []

        ProtNamed (_,types,_) -> 
            concat $ map getTypeVars types 

        CoProtNamed (_,types,_) -> 
            concat $ map getTypeVars types 

        ProtTensor (type1,type2,_) ->
            getTypeVars type1 ++
            getTypeVars type2 

        ProtPar    (type1,type2,_) ->
            getTypeVars type1 ++
            getTypeVars type2 

        ProtProc (types1,types2,types3,_) ->
            (concat.map getTypeVars) types1 ++
            (concat.map getTypeVars) types2 ++
            (concat.map getTypeVars) types3 


strToTVar :: [String] -> PosnPair -> [Type] 
strToTVar args dposn = map (\x -> TypeVar (x,dposn)) args

freeVars :: Type -> [Int]
freeVars texpr 
        = case texpr of
              TypeVarInt x -> 
                  [x]

              TypeFun (tins,tout,posn) ->
                  nub $ concat $ map freeVars (tout:tins)

              TypeDataType (name,dins,posn) ->
                  nub $ concat $ map freeVars dins 

              TypeCodataType (name,dins,posn) ->
                  nub $ concat $ map freeVars dins 

              TypeProd (types,pn) -> 
                  nub $ concat $ map freeVars types 

              Get (type1,type2,_) ->
                  nub $ concat $ map freeVars [type1,type2]

              Put (type1,type2,_) -> 
                  nub $ concat $ map freeVars [type1,type2]

              Neg (type1,_) -> 
                  nub $ freeVars type1

              ProtTensor (type1,type2,_) ->
                  nub $ concat $ map freeVars [type1,type2]

              ProtPar (type1,type2,_) ->
                  nub $ concat $ map freeVars [type1,type2]

              ProtProc (types1,types2,types3,_) ->
                  nub (
                       (concat.map freeVars) types1 ++
                       (concat.map freeVars) types2 ++
                       (concat.map freeVars) types3
                     )

              ProtNamed (name,pins,posn) ->
                  nub $ concat $ map freeVars pins 

              CoProtNamed (name,cpins,posn) ->
                  nub $ concat $ map freeVars cpins 

              otherwise ->
                  []   


printPosn :: PosnPair -> String
printPosn (line,col) = " at line,column (" ++ show line ++ "," ++ show col ++ ")"

-- ================================================================================
-- ================================================================================
-- ================================================================================

getDefnPosn :: Defn -> PosnPair
getDefnPosn defn 
        = case defn of  
              Data   (_,pn) ->
                  pn  
              Codata (_,pn) ->
                  pn 
              TypeSyn (_,pn) ->   
                  pn  
              ProtocolDefn   (_,pn) ->
                  pn  
              CoprotocolDefn (_,pn) ->
                  pn   
              FunctionDefn (_,_,_,pn) ->
                  pn  
              ProcessDefn  (_,_,_,pn) -> 
                  pn 
              TermSyn (_,_,pn) -> 
                  pn 
              OperatorDefn (_,_,pn) ->
                  pn               

getTypePosn :: Type -> PosnPair
getTypePosn cType 
        = case cType of 
              Unit pn ->
                  pn 
              TypeDataType  (_,_,pn) ->
                  pn 
              TypeCodataType(_,_,pn) -> 
                  pn 
              TypeProd  (_,pn)  -> 
                  pn 
              TypeConst (_,pn)  -> 
                  pn 
              TypeVar   (_,pn)  -> 
                  pn 
              TypeVarInt _      -> 
                  (0,0) 
              TypeFun   (_,_,pn)-> 
                  pn 
              Get (_,_,pn) ->
                 pn
              Put (_,_,pn) -> 
                 pn 
              Neg (_,pn) -> 
                 pn 
              ProtTensor (_,_,pn) ->
                 pn 
              ProtPar (_,_,pn) ->
                 pn 
              ProtProc (_,_,_,pn) ->
                 pn 
              TopBot pn  ->
                 pn 
              ProtNamed (_,_,pn) ->
                 pn   
              CoProtNamed (_,_,pn) ->
                 pn 

getTermPosn :: Term -> PosnPair
getTermPosn term 
        = case term of 
              TRecord list -> (\(a,b,c) -> c) (head list)
              TCallFun (_,_,pn) -> pn 
              TLet (_,_,pn)     -> pn 
              TVar (_,pn)       -> pn 
              TConst(_,pn)      -> pn 
              TIf (_,_,_,pn)    -> pn 
              TCase (_,_,pn)    -> pn 
              TFold (_,_,pn)    -> pn 
              TUnfold (_,_,pn)  -> pn 
              TCons (_,_,pn)    -> pn 
              TDest (_,_,pn)    -> pn 
              TProd (_,pn)      -> pn 
              TDefault pn       -> pn 

getPattPosn :: Pattern -> PosnPair
getPattPosn patt 
        = case patt of 
              ConsPattern (_,_,pn) -> pn 
              DestPattern (_,_,pn) -> pn 
              ProdPattern (_,pn)   -> pn  
              VarPattern  (_,pn)   -> pn 
              StrConstPattern(_,pn)-> pn 
              IntConstPattern(_,pn)-> pn  
              DontCarePattern pn   -> pn 
              NoPattern       pn   -> pn     

getProcPosn :: ProcessCommand -> PosnPair 
getProcPosn comm 
        = case comm of 
              PRun   (_,_,_,_,pn) -> 
                  pn 
              PClose (_,pn) -> 
                  pn 
              PHalt  (_,pn) ->
                  pn 
              PGet   (_,_,pn) ->
                  pn  
              PPut   (_,_,pn) ->
                  pn  
              PHPut  (_,_,pn) ->
                  pn  
              PHCase (_,_,pn) -> 
                  pn
              PSplit (_,_,pn) ->
                  pn
              PFork  (_,_,pn) ->
                  pn
              PPlug  (_,_,pn)   -> 
                  pn
              PId    (_,_,pn) -> 
                  pn
              PCase  (_,_,pn) ->
                  pn 
              PNeg (_,_,pn) -> 
                  pn 
-- ================================================================================
-- ================================================================================
-- ================================================================================

 -- Take a type with TypeVar String and change the variables to TypeVarInt Int 
renameFunType :: FunType -> 
                 ExceptT 
                   ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) FunType
renameFunType funType = do 
        case funType of 
            StrFType (uVars,ftype) -> do 
                uvarInts <- genNewVarList (length uVars)
                let 
                  substList = zip uVars uvarInts
                  newFType  = renameTVar substList ftype
                return $ IntFType (uvarInts,newFType)
            
            otherwise -> 
                return funType    


intTypeToStrType :: FunType -> Either ErrorMsg FunType
intTypeToStrType funType = do 
        case funType of 
            IntFType (uvars,fType) -> do 
                  let  
                     uVarsStrs = map (\x -> "T" ++ show x)
                                     [0,1..(length uvars-1)]
                     substList = zip uvars uVarsStrs 
                  case renameTVarInts substList fType of 
                      Just newFType -> 
                          return $ StrFType (uVarsStrs,newFType)
                      Nothing ->
                         Left $ ". Error renaming ::" ++ show funType     
            otherwise ->
                 return funType  

renameTVarInts :: [(Int,String)] -> Type -> Maybe Type 
renameTVarInts substList intType
    = case intType of 
          Unit pn -> 
              return $
                  Unit pn 

          TypeDataType (name,dtypes,posn) -> do 
              mDats <- mapM (renameTVarInts substList) dtypes
              return $ TypeDataType (name,mDats,posn)

          TypeCodataType (name,dtypes,posn) -> do 
              mCoDats <- mapM (renameTVarInts substList) dtypes
              return $ TypeCodataType (name,mCoDats,posn)
          
          TypeProd (types,posn) -> do 
              mProds <- mapM (renameTVarInts substList) types
              return $ TypeProd (mProds,posn)  

          TypeConst (bType,posn) ->
              return intType

          TypeVar (var,posn) ->
              return $ TypeVar (var,posn)

          TypeVarInt num ->
              case lookup num substList of 
                  Nothing ->
                      Nothing
                  Just sval -> 
                      return $ TypeVar(sval,(0,0))


          TypeFun (itypes,otype,posn) -> do 
              miTypes <- mapM (renameTVarInts substList) itypes
              moType  <- renameTVarInts substList otype
              return $ TypeFun (miTypes,moType,posn)

          Get (type1,type2,pn) -> do 
              nType1 <- renameTVarInts substList type1
              nType2 <- renameTVarInts substList type2 
              return $ Get (nType1,nType2,pn) 

          Put (type1,type2,pn) -> do 
              nType1 <- renameTVarInts substList type1
              nType2 <- renameTVarInts substList type2 
              return $ Put (nType1,nType2,pn)

          Neg (type1,pn) -> do 
              nType1 <- renameTVarInts substList type1
              return $ Neg (nType1,pn) 

          ProtNamed (name,dtypes,posn) -> do 
              mprots <- mapM (renameTVarInts substList) dtypes
              return $ ProtNamed (name,mprots,posn)

          CoProtNamed (name,dtypes,posn) -> do 
              mcoProts <- mapM (renameTVarInts substList) dtypes
              return $ CoProtNamed (name,mcoProts,posn)

          ProtTensor (type1,type2,pn) -> do 
              nType1 <- renameTVarInts substList type1
              nType2 <- renameTVarInts substList type2 
              return $ ProtTensor (nType1,nType2,pn)

          ProtPar (type1,type2,pn) -> do 
              nType1 <- renameTVarInts substList type1
              nType2 <- renameTVarInts substList type2 
              return $ ProtPar (nType1,nType2,pn)

          ProtProc (types1,types2,types3,pn) -> do 
              nTypes1 <- mapM (renameTVarInts substList) types1
              nTypes2 <- mapM (renameTVarInts substList) types2  
              nTypes3 <- mapM (renameTVarInts substList) types3
              return $ ProtProc (nTypes1,nTypes2,nTypes3,pn)

          otherwise -> do 
              return intType  




renameTVar :: [(String,Int)] -> Type -> Type 
renameTVar substList typeflem   
    = case typeflem of
          Unit pn -> 
              Unit pn 

          TypeDataType (name,dtypes,posn) ->
              TypeDataType 
                  (
                    name,
                    map (renameTVar substList) dtypes,
                    posn
                   )

          TypeCodataType (name,dtypes,posn) ->
              TypeCodataType 
                  (
                    name,
                    map (renameTVar substList) dtypes,
                    posn
                  )
          
          TypeProd (types,posn) ->
              TypeProd (map (renameTVar substList) types,posn)  

          TypeConst (bType,posn) ->
              typeflem

          TypeVar (var,posn) ->
              case lookup var substList of 
                  Just varInt -> 
                      TypeVarInt varInt
                  Nothing ->
                      error 
                        "This means the universal var list for function type was not accurate."

          TypeVarInt num ->
              typeflem

          TypeFun (itypes,otype,posn) ->
              TypeFun (
                        map (renameTVar substList) itypes,
                        renameTVar substList otype,
                        posn
                      ) 

          Get (type1,type2,pn) ->
              Get (
                   renameTVar substList type1,
                   renameTVar substList type2,
                   pn
                  )

          Put (type1,type2,pn) -> 
              Put (
                   renameTVar substList type1,
                   renameTVar substList type2,
                   pn
                  )

          Neg (type1,pn) -> 
              Neg (
                   renameTVar substList type1,
                   pn
                  )

          ProtTensor (type1,type2,pn) ->
              ProtTensor
                 (
                   renameTVar substList type1,
                   renameTVar substList type2,
                   pn
                 )

          ProtPar (type1,type2,pn) ->
              ProtPar
                 (
                   renameTVar substList type1,
                   renameTVar substList type2,
                   pn
                 )

          ProtProc (types1,types2,types3,pn) ->
              ProtProc
                 (
                   map (renameTVar substList) types1,
                   map (renameTVar substList) types2,
                   map (renameTVar substList) types3,
                   pn
                 )

          ProtNamed (name,dtypes,posn) ->
              ProtNamed 
                  (
                    name,
                    map (renameTVar substList) dtypes,
                    posn
                   )

          CoProtNamed (name,dtypes,posn) ->
              CoProtNamed 
                  (
                    name,
                    map (renameTVar substList) dtypes,
                    posn
                   )

          otherwise ->
              typeflem 
                   
-- =====================================================================================
-- =====================================================================================


genNewVar :: ExceptT ErrorMsg 
                  (State (Int,TypeThing,Context,ChanContext,SymbolTable)) Int 
genNewVar = do
    (num,_,_,_,_) <- get 
    modify $ \(x,t,c,chC,st) -> (x + 1,t,c,chC,st)
    return num 

-- =====================================================================================
-- =====================================================================================


genNewVarList :: Int -> 
                 ExceptT ErrorMsg
                    (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [Int]
genNewVarList 0 =
    return [] 
genNewVarList n = do 
    v  <- genNewVar 
    vs <- genNewVarList (n-1)
    return (v:vs)

-- =====================================================================================
-- =====================================================================================

tvarToStr :: Term -> String
tvarToStr (TVar pair) = fst pair 

getCommName :: ProcessCommand -> String 
getCommName comm 
        = case comm of 
              PRun   (_,_,_,_,pn) -> 
                  genCommMsg ("Run",pn) 

              PClose (_,pn) -> 
                  genCommMsg ("Close",pn) 

              PHalt  (_,pn) ->
                  genCommMsg ("Halt",pn) 

              PGet   (_,_,pn) ->
                  genCommMsg ("Get",pn)  

              PPut   (_,_,pn) ->
                  genCommMsg ("Put",pn)  

              PHPut  (_,_,pn) ->
                  genCommMsg ("HPut",pn)  

              PHCase (_,_,pn) -> 
                  genCommMsg ("HCase",pn)

              PSplit (_,_,pn) ->
                  genCommMsg ("Split",pn)

              PFork  (_,_,pn) ->
                  genCommMsg ("Fork",pn)

              PPlug  (_,_,pn)  -> 
                  genCommMsg ("Plug",pn)

              PId    (_,_,pn) -> 
                  genCommMsg ("Id",pn)

              PCase  (_,_,pn) ->
                  genCommMsg ("Case",pn)

              PNeg (_,_,pn) -> 
                  genCommMsg ("Neg",pn)


genCommMsg :: (String,PosnPair) -> String
genCommMsg (str,pn)
    = "<<" ++ str ++ " Command>>" 
       ++ printPosn pn 

-- ================================================================
-- ================================================================

fromFunType :: FunType -> ([String],Type)
fromFunType (StrFType (foralls,sType))
    = (foralls,sType)

getAllFunNames :: [Defn] -> [FuncName]
getAllFunNames defns = map getFunDefnName defns 

getFunDefnName :: Defn -> FuncName 
getFunDefnName (FunctionDefn (fn,_,_,_)) = fn 

getAllProcNames :: [Defn] -> [Name]
getAllProcNames defns = map getProcDefnName defns 

getProcDefnName :: Defn -> Name 
getProcDefnName (ProcessDefn (pname,_,_,_)) = pname 

-- ===================================================================================
-- ===================================================================================
combineEqns :: [TypeEqn] -> [TypeEqn]
combineEqns totEqns
        = case  noQuantEqns totEqns of 
              True  ->
                  totEqns
              False ->
                  combineEqnsHelper totEqns ([],[],[])

combineEqnsHelper :: [TypeEqn] -> (UniVars,ExistVars,[TypeEqn]) -> [TypeEqn]
combineEqnsHelper [] (suv,sev,steqns)
        = [TQuant (suv,sev) steqns]
combineEqnsHelper (eqn:eqns) (suv,sev,steqns)  
        = case eqn of 
             TSimp simpEqn ->
                 combineEqnsHelper eqns (suv,sev,(steqns ++ [TSimp simpEqn]))
             TQuant (uvars,evars) eqlist ->
                 combineEqnsHelper eqns (suv++uvars,sev++evars,steqns++eqlist) 

combinePattProcEqns :: [TypeEqn] -> [TypeEqn] -> [TypeEqn]
combinePattProcEqns pattEqns procEqns 
    = case null pattEqns of 
          True -> 
              procEqns
          False ->
              case head pattEqns of 
                   TQuant (uvars,evars) qeqns ->
                       [TQuant (uvars,evars) (qeqns ++ procEqns)]
                   otherwise ->
                       pattEqns ++ procEqns


combEqns_Proc :: ([TypeEqn],[TypeEqn]) -> [TypeEqn]
combEqns_Proc (ts,teqns) 
    = case null ts of 
          True  -> 
              teqns
          False -> 
              case hts of 
                  TQuant (uvars, evars) tqs ->
                      [TQuant (uvars,evars) (tqs++teqns)]
                  TSimp (_,_) -> 
                      hts:teqns
            where
               hts = head ts  

-- return True if there is no Quant eqn
noQuantEqns :: [TypeEqn] -> Bool 
noQuantEqns eqns 
        = case (filter isQuantEqn eqns) of 
              [] -> 
                  True
              _  ->
                  False    

isQuantEqn :: TypeEqn -> Bool
isQuantEqn eqn 
        = case eqn of 
              TQuant _ _ ->
                  True
              TSimp _ ->
                  False   


isProtData :: Defn -> Bool 
isProtData defn = case defn of 
        Data   _ -> 
            True
        Codata _ ->
            True
        ProtocolDefn _ ->
            True
        CoprotocolDefn _ ->
            True
        otherwise ->
            False     
  

putStrLnRed :: String -> IO ()
putStrLnRed str = do 
  --setSGR [SetColor Foreground Vivid Red]
  putStrLn str 

-- ======================================================================================
-- ======================================================================================


subsInTerm :: (Term,Term) -> Term -> Term  
subsInTerm subst@(oTerm,nTerm) sterm 
    = case sterm of
        TRecord tripList -> 
          TRecord (substTripList subst tripList) 
        
        TCallFun (fName,terms,pn) ->
          TCallFun (fName,map (subsInTerm subst) terms,pn)

        TLet (term,letwhrs,pn) -> 
          TLet (
                subsInTerm subst term,
                map (substLetWhr subst) letwhrs,
                pn 
               ) 

        TVar (str,pn) -> 
          case ostr == str of 
            True  -> 
              --error $ "hey this is " ++ show subst 
              nTerm
            False ->
              sterm

        TIf     (t1,t2,t3,pn) -> 
          TIf (
                subsInTerm subst t1,
                subsInTerm subst t2,
                subsInTerm subst t3,
                pn
              )

        TCase   (term,pattTerms,pn) -> 
          TCase (
                 subsInTerm subst term,
                 map (susbtInPattCase subst) pattTerms,
                 pn
                )

        TFold   (term,foldPatts,pn) -> 
          TFold (
                  subsInTerm subst term,
                  map (substInFoldPatt subst) foldPatts,
                  pn
                ) 

        TUnfold (term,foldPatt,pn) -> 
          undefined

        TCons   (nm,terms,pn) -> 
          TCons (nm, map (subsInTerm subst) terms,pn)

        TDest (nm,terms,pn) -> 
          TDest (nm,map (subsInTerm subst) terms,pn)

        TProd (terms,pn) -> 
          TProd (map (subsInTerm subst) terms,pn)

        otherwise -> 
          sterm  
    where 
       TVar (ostr,opn) = oTerm 

-- ===================================================================
-- ===================================================================

substTripList :: (Term,Term) -> [(Pattern,Term,PosnPair)] -> 
                 [(Pattern,Term,PosnPair)]

substTripList _ []
    = []
substTripList subst ((patt,term,pn):rest)
    = ((patt,subsInTerm subst term,pn):substTripList subst rest) 


substInFoldPatt :: (Term,Term) -> FoldPattern -> FoldPattern
substInFoldPatt subst (nm,patts,term,pn)
    = (nm,patts,subsInTerm subst term,pn) 


substInFun :: (Term,Term) -> [(PatternTermPhr,PosnPair)] -> 
              [(PatternTermPhr,PosnPair)]
substInFun subst  
    = map (\(pt,ppn) -> (susbtInPattCase subst pt,ppn)) 


susbtInPattCase :: (Term,Term) -> PatternTermPhr -> PatternTermPhr
susbtInPattCase subst (patts,Left term)
    = (patts,Left (subsInTerm subst term))

-- ========================================================================
-- ========================================================================


substLetWhr :: (Term,Term) -> LetWhere -> LetWhere

substLetWhr subst (LetPatt (patt,term)) 
    = LetPatt (patt,subsInTerm subst term)
substLetWhr subst letDefn 
    = substInDefn subst letDefn   


substInDefn :: (Term,Term) -> LetWhere -> LetWhere 
substInDefn subst oDef@(LetDefn defn) 
    = case defn of 
        FunctionDefn (fnm,fType,fbody,pn) ->
            LetDefn newDefn
          where 
            newDefn = FunctionDefn (fnm,fType,substInFun subst fbody,pn)

        otherwise -> 
            oDef 
