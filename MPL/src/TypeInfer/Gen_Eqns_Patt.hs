module TypeInfer.Gen_Eqns_Patt where 

import TypeInfer.MPL_AST
import TypeInfer.Gen_Eqns_CommFuns
import TypeInfer.SymTab_DataType
import TypeInfer.SymTab 

import Control.Monad.State.Lazy
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint
import Data.List 
import Control.Monad.Except
import Data.Maybe 

genPattEquationsList ::[Pattern] -> [TypeThing] -> 
                       ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                       [TypeEqn]
genPattEquationsList [] [] 
        = return [] 
genPattEquationsList (t:ts) (nvar:rest) = do
        (_,_,context,_,_) <- get 
        modify $ \(n,v,c,chC,st) -> (n,nvar,c,chC,st)
        eqns <- genPattEqns t  
        eqnsList <- genPattEquationsList ts rest 
        return $ combineEqns (eqns++eqnsList)   

genPattEquationsList patts tthing 
        = error $ "\n" ++ show patts ++ "\n" ++ show tthing

fold_Pattern :: ((String,[Pattern],PosnPair) -> b) ->
                ((String,[Pattern],PosnPair) -> b) ->
                (([Pattern],PosnPair) -> b) ->
                ((String,PosnPair) -> b) ->
                ((String,PosnPair) -> b) ->
                ((Int,PosnPair) -> b) ->
                (PosnPair -> b) ->
                (PosnPair -> b) ->
                Pattern -> b  

fold_Pattern pattCons pattDest pattprod pattvar pattconstStr 
             pattconstInt pattdcare pattNo p  
        = case p of
              ConsPattern (str,patts,posn) ->
                  pattCons (str,patts,posn)
              
              DestPattern (str,patts,posn) ->
                  pattDest (str,patts,posn)

              ProdPattern (patts,posn) ->
                  pattprod (patts,posn)

              VarPattern (str,posn) ->
                  pattvar (str,posn)

              StrConstPattern (str,posn) ->
                 pattconstStr (str,posn)

              IntConstPattern(num,posn) ->
                 pattconstInt (num,posn) 

              DontCarePattern posn ->
                 pattdcare posn 

              NoPattern pn ->
                pattNo pn  


genPattEqns :: Pattern -> 
               ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
genPattEqns patt 
        = fold_Pattern funPattCons funPattDest funPattProd funPattVar 
                       funConstStringPattern funConstIntPattern 
                       funPattDCare funNoPatt patt   

-- =========================================================================================
-- ========================================================================================= 

funPattCons :: (String,[Pattern],PosnPair) -> 
               ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
funPattCons (consName,patts,posn) = do
        (_,typePattCons,context,_,symTab) <- get 
        let 
          eithVal = lookup_ST (Val_Cons (consName,posn)) symTab
        case eithVal of 
            Right retVal -> do 
                let 
                  ValRet_Cons ((datName,allConses),ftype,_,nargs)
                           = retVal 
                helperPattCons patts (consName,posn) (ftype,nargs)    
            Left emsg -> do 
                liftEither $ Left emsg


helperPattCons :: [Pattern] -> (String,PosnPair)  -> (FunType,NumArgs) -> 
                  ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]

helperPattCons patts (consName,posn) (ftype,nargs) = do 
        (_,typePattCons,context,_,symTab) <- get 
        case nargs == length patts of
            True  -> do 
                renFunType <- renameFunType ftype 
                newVars    <- genNewVarList nargs
                pattEqns   <- genPattEquationsList patts newVars
                let 
                  (univVars,itypes,otype,sposn)
                          = stripFunType renFunType posn 1
                  outEqn  = TSimp (TypeVarInt typePattCons,otype)
                  inEqns  = zipWith (\x y -> TSimp (TypeVarInt y,x)) itypes newVars
                  finEqns = combineEqns 
                                ((TQuant ([],univVars++newVars) (outEqn:inEqns)):pattEqns)
                                   --(combineEqns(pattEqns ++ outEqn:inEqns)) 
                return finEqns 

            False -> do 
                let
                  emsg 
                    = "Constructor <<" ++ consName ++ ">> " ++ printPosn posn ++
                      "called with incorrect number of arguments.Expected " 
                       ++ show nargs ++ " ,got " ++ show (length patts)
                liftEither $ Left emsg    

-- =========================================================================================
-- ========================================================================================= 

funPattDest :: (String,[Pattern],PosnPair) -> 
               ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
funPattDest (consName,patts,posn) = do
        (_,_,context,_,symTab) <- get 
        let 
          eithVal = lookup_ST (Val_Dest (consName,posn)) symTab
        case eithVal of
            Right retVal -> do 
                let 
                  ValRet_Dest ((codatName,allDests),ftype,nargs)
                           = retVal 
                helperPattDest patts (consName,posn) (ftype,nargs)
            Left emsg ->
                liftEither $ Left emsg            


helperPattDest :: [Pattern] -> (String,PosnPair) -> (FunType,NumArgs) -> 
                  ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
                                  

helperPattDest patts (destName,posn) (funType,nargs) = do 
        (_,typePattDest,context,_,symTab) <- get 
        case nargs-1 == length patts of
            True  -> do 
                renFunType <- renameFunType funType 
                newVars    <- genNewVarList (nargs-1)
                pattEqns   <- genPattEquationsList patts newVars
                let 
                  (univVars,itypes,otype,sposn)
                            = stripFunType renFunType posn 1
                  iTypesWORec
                            = init itypes 
                  tFunSTab  = TypeFun (itypes,otype,sposn)
                  dEqn      = TSimp (TypeVarInt typePattDest,tFunSTab)
                  inEqns    = zipWith (\x y -> TSimp (TypeVarInt x,y)) newVars iTypesWORec
                  finEqn    = TQuant ([],univVars++newVars) 
                                     (dEqn:(inEqns ++ pattEqns)) 
                return [finEqn]                    

            False -> do 
                let
                  emsg 
                    = "Destructor <<" ++ destName ++ ">> " ++ printPosn posn ++
                      "called with incorrect number of arguments.Expected " 
                       ++ show nargs ++ " ,got " ++ show (length patts)
                liftEither $ Left emsg 

stripFunType :: FunType -> PosnPair -> Int -> ([Int],[Type],Type,PosnPair)
stripFunType funType nposn flag
        = case funType of 
              IntFType (uvars,ifunType) -> 
                  case ifunType of
                      TypeFun (itypes,otype,posn) ->
                          case flag == 1 of
                              True ->
                                  (uvars,map (changePosn nposn) itypes,
                                   changePosn nposn otype,posn)
                              False ->  
                                  (uvars,itypes,otype,posn)    
                      otherwise ->
                          error $ show funType 

              otherwise ->
                  error $ "Not expecting a function type like this::" ++ show funType     

stripProcProt :: FunType -> ([Int],[Type],[Type],[Type],PosnPair)
stripProcProt funType
        = case funType of 
              IntFType (uvars,procType) -> 
                  case procType of
                      ProtProc (seqTypes,itypes,otypes,posn) ->
                          (uvars,seqTypes,itypes,otypes,posn)    
                      otherwise ->
                          error $ show funType 

              otherwise ->
                  error $ "Not expecting a process type like this::" ++ show funType     



changePosn :: PosnPair ->Type -> Type 
changePosn nposn stype
        = case stype of 
              TypeDataType   (dn,dins,opn)   ->
                  TypeDataType (dn,map (changePosn nposn) dins,nposn) 
              TypeCodataType (cdn,cdins,opn) ->
                  TypeCodataType (cdn,map (changePosn nposn) cdins,nposn)
              otherwise -> 
                  stype 


-- =========================================================================================
-- ========================================================================================= 
funPattProd :: ([Pattern],PosnPair) ->
               ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
funPattProd (patts,posn) = do  
        (_,typePattProd,context,_,symTab) <- get  
        newVars  <- genNewVarList (length patts)
        pattEqns <- genPattEquationsList patts newVars 
        let
          prodEqn = TSimp (TypeVarInt typePattProd,TypeProd (map TypeVarInt newVars,posn))  
          finEqn  = TQuant ([],newVars) (combineEqns (prodEqn:pattEqns)) 
        return [finEqn]                      
-- =========================================================================================
-- ========================================================================================= 
--update the context with the variable
funPattVar :: (String,PosnPair) ->
              ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]  
funPattVar (str,posn) = do 
        (_,typePattVar,context,_,symTab) <- get 
        let 
          newContext = insertCtxt (str,TypeVarInt typePattVar) context
        modify $ \(n,tt,c,chC,st) -> (n,tt,newContext,chC,st)
        return []  


insertCtxt :: (String,Type) -> Context -> Context
insertCtxt (str,ctype) []
        = [[(str,ctype)]]
insertCtxt (str,ctype) (c:cs)
        = ((str,ctype):c):cs  

                         

-- =========================================================================================
-- ========================================================================================= 
funConstStringPattern :: (String,PosnPair) -> 
                   ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn] 
funConstStringPattern (constStr,posn) = do 
        (_,typePattConst,context,_,symTab) <- get 
        let 
          eqn = TSimp (TypeVarInt typePattConst,TypeConst (BaseString,posn))
        return [eqn]  

-- =========================================================================================
-- ========================================================================================= 
funConstIntPattern :: (Int,PosnPair) -> 
                   ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn] 
funConstIntPattern (constStr,posn) = do 
        (_,typePattConst,context,_,symTab) <- get 
        let 
          eqn = TSimp (TypeVarInt typePattConst,TypeConst (BaseInt,posn))
        return [eqn]  

-- =========================================================================================
-- ========================================================================================= 
funPattDCare :: PosnPair -> ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
funPattDCare posn = do  
        return []

-- =========================================================================================
-- ========================================================================================= 
funNoPatt :: PosnPair -> ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
funNoPatt posn = do  
        (_,typeNoPatt,context,_,symTab) <- get 
        let 
          eqn = TSimp (TypeVarInt typeNoPatt,Unit posn)
        return [eqn]  

-- =========================================================================================
-- ========================================================================================= 

getFunVar :: FunType -> ([String],Type)
getFunVar (StrFType (uVars,ftype)) = (uVars,ftype)


-- This function returns the list of all global variables ,
-- fold function types for all the constructors as well the output
-- type of the fold function which is the same for all the fold functions
-- of a data type.

renameFunsTypes :: [FunType] -> 
                  ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                                   ([Int],[Type],Type)
renameFunsTypes funs = do 
        (allVars,alltypes,otype) <- rnmFTypesHelp funs
        uvarInts <- genNewVarList (length allVars)  
        let 
          substList = zip allVars uvarInts
          allIntTypes
                    = map (renameTVar substList) alltypes 
          oIntType  = renameTVar substList otype
        return (uvarInts,allIntTypes,oIntType)  
                   

rnmFTypesHelp :: [FunType] -> 
                  ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                                   ([String],[Type],Type)
rnmFTypesHelp allFuns = do 
        let 
          allVars   = (nub.concat.map (fst.getFunVar)) allFuns
          alltypes  = (map (snd.getFunVar)) allFuns
          (_,oType) = getOutFnType (head alltypes)
        return (allVars,alltypes,oType)

getOutFnType :: Type -> ([Type],Type) 
getOutFnType (TypeFun (ins,out,pn)) = (ins,out) 
                         
