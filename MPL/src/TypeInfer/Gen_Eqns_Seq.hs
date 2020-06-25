module TypeInfer.Gen_Eqns_Seq where 

import TypeInfer.Gen_Eqns_Patt
import TypeInfer.MPL_AST
import TypeInfer.Gen_Eqns_CommFuns
import TypeInfer.SymTab
import TypeInfer.SymTab_DataType 
import TypeInfer.SolveEqns

import Control.Monad.State.Lazy
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint
import Data.List 
import Control.Monad.Except
import Data.Maybe



-- =========================================================================================
-- =========================================================================================  

genEquations :: Term ->
                ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                [TypeEqn]
genEquations term = foldTerm 
                        fun_Record fun_CallFun fun_Let fun_Var fun_Const 
                        fun_If fun_Case fun_Fold fun_Unfold fun_Cons
                        fun_Dest fun_Prod fun_Default term 

-- here the thing to note is that context is not restored once the equation has been generated
-- for the first element. Keep in mind that context  needs to be restored in genEquation itself.
genEquationsList :: [Term] -> [TypeThing] -> 
                    ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
genEquationsList [] [] 
        = return []
genEquationsList (t:ts) (nvar:rest) = do
        (_,_,context,_,_) <- get 
        modify $ \(n,v,c,chC,st) -> (n,nvar,c,chC,st)
        eqns <- genEquations t  
        eqnsList <- genEquationsList ts rest 
        return (eqns++eqnsList)   

genEquationsList a b = error $ "The error is :" ++ show a ++ "\n" ++ show b
-- =========================================================================================
-- =========================================================================================  

foldTerm :: ([(Pattern,Term,PosnPair)] -> b) ->
            ((FuncName,[Term],PosnPair) -> b) -> 
            ((Term,[LetWhere],PosnPair) -> b) ->
            ((String,PosnPair) -> b) ->
            ((BaseVal,PosnPair) -> b) ->
            ((Term,Term,Term,PosnPair) -> b) ->
            ((Term,[PatternTermPhr],PosnPair) -> b) ->
            ((Term,[FoldPattern],PosnPair) -> b) ->
            ((Term,FoldPattern,PosnPair) -> b) ->
            ((Name,[Term],PosnPair) -> b) ->
            ((Name,[Term],PosnPair) -> b) ->
            (([Term],PosnPair) -> b) ->
            (PosnPair -> b) ->
            Term -> b 

foldTerm frecord fcallFun flet fvar fconst fif fcase
         ffold f_unfold fCons fdest fProd fDef 
         t  
    = case t of 
         TRecord reclist -> 
             frecord reclist                

         TCallFun(fname,terms,posn) ->  
             fcallFun (fname,terms,posn)

         TLet(term,letwhrs,posn) ->
             flet (term,letwhrs,posn)

         TVar(str,posn) ->   
             fvar (str,posn)

         TConst(bval,posn) ->
             fconst (bval,posn)

         TIf(term1,term2,term3,posn) ->
             fif (term1,term2,term3,posn)

         TCase(term,pattTermphrs,posn) ->
             fcase (term,pattTermphrs,posn) 
             
         TFold(term,foldPatt,posn) ->
             ffold (term,foldPatt,posn)

         TUnfold(term,foldPatt,posn) ->
             f_unfold (term,foldPatt,posn)

         TCons(name,terms,posn) ->
            fCons (name,terms,posn)
        
         TDest(name,terms,posn) ->
            fdest (name,terms,posn) 

         TProd(terms,posn) ->
            fProd (terms,posn) 

         TDefault posn ->
            fDef posn 

-- =========================================================================================
-- =========================================================================================  

fun_Record :: [(Pattern,Term,PosnPair)] ->
              ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
fun_Record reclist = do                
        (_,typeRecord,context,_,symTab) <- get 
        let 
          (destName,dposn)
                  = (fsttrdTriple.head) reclist 
          allNamesPosn
                  = map fsttrdTriple reclist
          eithVal = lookup_ST (Val_Dest (destName,dposn)) symTab           
        case eithVal of 
            Right retVal -> do 
                let 
                  ValRet_Dest ((cdatName,allDests),ftype,nargs)
                             = retVal 
                  eithRecord = checkRecords allNamesPosn allDests cdatName         
                case eithRecord of 
                    Left emsgR ->
                        throwError emsgR
                    Right true -> do 
                        handleListRec reclist
            
            Left emsg ->
                throwError emsg     

-- first check if there are any extra destructors
-- then check if all the destructors are used
checkRecords :: [(Name,PosnPair)] -> [Name] -> DataName ->  Either ErrorMsg Bool 
checkRecords recDestsPn codDests codata = do 
        let
          recDests    = map fst recDestsPn  
          absentDests = codDests \\ recDests
          dupDests    = recDests \\ (nub recDests)
          extraDests  = recDests \\ codDests
        case extraDests == [] of 
            True  -> do 
                case absentDests == [] of
                    True  ->   
                        case dupDests == [] of 
                            True  ->
                                return True
                            False -> do 
                                let 
                                  dupDestsPosn
                                       = filter (\(x,p) -> elem x dupDests) recDestsPn
                                  emsg = "Following destructors have been used more than once in" ++
                                         "a record of Codata <<" 
                                          ++ show codata ++ ">>.\n" ++ 
                                          (intercalate "\n" (map printConsDest dupDestsPosn))
                                Left emsg 

                    False -> do 
                        let 
                          absDestPosn
                               = filter (\(x,p) -> elem x absentDests) recDestsPn
                          emsg = "Following destructors missing for Codata <<" 
                                 ++ show codata ++ ">>.\n" ++ 
                                 (intercalate "\n" (map printConsDest absDestPosn))
                        Left emsg                           
            False -> do 
                let 
                  extDestPosn
                       = filter (\(x,p) -> elem x extraDests) recDestsPn
                  emsg = "Extraneous destructors found.Following destructors don't belong to Codata <<" 
                         ++ show codata ++ ">>.\n" ++
                          (intercalate "\n" (map printConsDest extDestPosn))
                Left emsg          


printConsDest :: (Name,PosnPair) -> String
printConsDest (name,posn) = show name ++ "(" ++ printPosn posn ++ ")"                  

fsttrdTriple :: (Pattern,Term,PosnPair) -> (Name,PosnPair)
fsttrdTriple (x,y,z) = (getNameFromPatt x,z)


getNameFromPatt :: Pattern -> Name 
getNameFromPatt patt 
        = case patt of 
              DestPattern (str,_,_) -> 
                  str 
              ConsPattern (str,_,_) ->
                  str 
              otherwise ->
                   error $ "Expecting a constructor or destructor pattern." 



handleListRec :: [(Pattern,Term,PosnPair)] -> 
                 ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
handleListRec []
        = return []

handleListRec (r:rs) = do 
        (_,typeRecord,context,_,symTab) <- get 
        newR  <- handleRec r 
        newRS <- handleListRec rs 
        let 
          combEqns = combineEqns (newR ++ newRS)
        modify $ \(n,v,c,chC,st) -> (n,typeRecord,context,chC,st)
        return combEqns



handleRec :: (Pattern,Term,PosnPair) -> 
             ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
handleRec (pattern,term,posn) = do 
        (_,typeRecord,context,_,symTab) <- get 
        typeDest <- genNewVar
        pattEqn  <- genPattEquationsList [pattern] [typeDest]
        typeTerm <- genNewVar
        -- in the changed context evaluate the term
        termEqns <- genEquationsList [term] [typeTerm]
        let 
           (uvars,evars,typeEqns) 
                 = removeQuant (head pattEqn)
           TypeFun (ins,out,tposn)
                 = (snd.removeSimp.head) typeEqns
           remEqns
                 = tail typeEqns
           recType
                 = last ins 
           recEqn 
                 = TSimp (TypeVarInt typeRecord,recType)
           outEqn= TSimp (TypeVarInt typeTerm,out) 
           finEqn= TQuant (uvars,typeTerm:evars)
                          (recEqn:outEqn:(remEqns ++ termEqns))       
        modify $ \(n,v,c,chC,st) -> (n,typeRecord,context,chC,st)
        return [finEqn]
        -- change back the context to original




removeQuant :: TypeEqn -> (UniVars,ExistVars,[TypeEqn])
removeQuant (TQuant (uvars,evars) teqns) 
        = (uvars,evars,teqns)

removeSimp :: TypeEqn -> (Type,Type)
removeSimp (TSimp (t1,t2)) = (t1,t2)
   

-- =========================================================================================
-- ========================================================================================= 

fun_CallFun :: (FuncName,[Term],PosnPair) -> 
               ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
               [TypeEqn]
fun_CallFun (fname,terms,posn) = do 
    (_,typeLFun,context,_,symTab) <- get
    case lookup_ST (Val_Fun (fname,posn)) symTab of 
      Right valRet -> do 
        let 
          argLen = length terms
        (fuvars,fintypes,strOutType,nargs) <- remove_ValRet_Fun valRet posn

        case (argLen == nargs) of 
          True  -> do 
            newVars  <- genNewVarList nargs
            termEqns <- genEquationsList terms newVars
            modify $ \(n,tt,c,chC,st) -> (n,tt,context,chC,st)
            let
              compOutEqn
                       = TSimp (TypeVarInt typeLFun,strOutType) 
              compEqns = zipWith (\x y -> TSimp (TypeVarInt y,x)) fintypes newVars
              finEqns  = TQuant ([],fuvars++newVars)
                                (compOutEqn:(compEqns++ termEqns))
            return [finEqns]  

          False -> do 
            let
              emsg 
                = "Function <<" ++ show fname ++ ">> " ++ printPosn posn ++
                  "called with incorrect number of arguments.Expected " 
                   ++ show nargs ++ " , got " ++ show argLen 
            throwError emsg  

      Left emsg    ->   
        throwError emsg


remove_ValRet_Fun :: ValRet -> PosnPair -> 
                     ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                     (UniVars,[Type],Type,NumArgs)

remove_ValRet_Fun (ValRet_Fun (ftype,nargs)) posn  = do 
        renFType <- renameFunType ftype
        let 
          (uVars,itypes,otype,sposn)
              = stripFunType renFType posn 1 
        case itypes == [Unit (0,0)] && nargs == 1 of 
          True ->
            return (uVars,itypes,otype,0)
          False -> 
            return (uVars,itypes,otype,nargs)


-- =========================================================================================
-- ========================================================================================= 
{-
Put all the defns (here the defns are function defns) in a new scope in the symbol table and 
then type infer the term.Once that is done restore the symbol table to the original.
-}

fun_Let :: (Term,[LetWhere],PosnPair) ->
           ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]

fun_Let (term,letwhrs,posn) = do 
        (initNum,typeLet,context,_,symTab) <- get 
        modify $ \(n,tt,c,chC,st) -> (n+1,tt+1,context,chC,symTab)
        let 
          ldefns    = filter isLetDefn letwhrs
          lremPatts = letwhrs \\ ldefns
          defns     = map removeLetDefn ldefns
          remPatts  = map removeLetPatt lremPatts 
        quadDefnList  <- takeCareofFunDefns defns 
        let 
          newDefns  = (\(a,b,c) -> a) quadDefnList
          newSymTab = insert_ST newDefns symTab NewScope
        -- patt,Terms will be evaluated in the old context and
        -- new symtab.once their evaluation is done the symtab it
        -- took in is returned along with an updated context in which
        -- the term part of the let is evaluated.   
        modify $ \(n,tt,c,chC,st) -> (n,typeLet,context,chC,newSymTab)
        thngList <- genNewVarList (length remPatts)
        pattEqns <- letPattTermEqns remPatts thngList
        termEqns <- genEquationsList [term] [typeLet]
        modify $ \(n,tt,c,chC,st) -> (n,tt,context,chC,symTab)
        let finEqn = TQuant ([],thngList) (combineEqns (pattEqns ++ termEqns))
        return [finEqn]


isLetDefn :: LetWhere -> Bool
isLetDefn ld = case ld of 
    LetDefn _ -> True
    otherwise -> False 

removeLetDefn :: LetWhere -> Defn 
removeLetDefn (LetDefn d) = d 

removeLetPatt :: LetWhere -> (Pattern,Term)
removeLetPatt (LetPatt (p,t)) = (p,t)


letPattTermEqns :: [(Pattern,Term)] -> [TypeThing] -> 
                   ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                   [TypeEqn] 
letPattTermEqns [] [] 
        = return [] 
letPattTermEqns ((patt,term):rest) (typeThing:ts) = do 
        (_,_,context,_,symTab) <- get
        pattEqns <- genPattEquationsList [patt] [typeThing]
        termEqns <- genEquationsList [term] [typeThing]
        remEqns  <- letPattTermEqns rest ts 
        return (pattEqns ++ termEqns ++ remEqns)




-- =========================================================================================
-- ========================================================================================= 

fun_Var :: (String,PosnPair) -> 
           ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
fun_Var (var,posn) = do      
        (num,typeVar,context,_,_) <- get 
        case lookup_ctxt var context of
            Nothing -> do 
                throwError 
                  $ "\nVariable <<" ++ show var ++ ">>" ++ 
                     printPosn posn ++ " not in scope.\n" 

            Just someType -> do 
               return [TSimp (TypeVarInt typeVar,someType)]


lookup_ctxt :: String -> Context -> Maybe Type 
lookup_ctxt var context
        = lookup var (concat context)   

-- =========================================================================================
-- ========================================================================================= 

fun_Const :: (BaseVal,PosnPair) -> 
             ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
fun_Const (bval,posn) = do 
        (num,typeConst,context,_,symTab) <- get
        let
          eqn = TSimp (
                       TypeVarInt typeConst,
                       TypeConst (baseValToBType bval,posn)
                       )
        return $ [eqn]  
  
baseValToBType :: BaseVal -> BaseType 
baseValToBType bval 
        = case bval of 
              ConstInt _  ->
                  BaseInt
              ConstChar _ ->
                  BaseChar
              ConstString _ -> 
                  BaseString
              ConstDouble _ ->
                  BaseDouble

-- =========================================================================================
-- ========================================================================================= 

fun_If :: (Term,Term,Term,PosnPair) ->
          ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
fun_If (term1,term2,term3,posn) = do 
       (num,typeIf,context,_,symTab) <- get 
       eqns1 <- genEquations term1
       eqns2 <- genEquations term2
       eqns3 <- genEquations term3
       return $ eqns1 ++ eqns2 ++ eqns3 


-- =========================================================================================
-- ========================================================================================= 


fun_Case :: (Term,[PatternTermPhr],PosnPair) ->
            ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
fun_Case (term,patttermphrs,posn) = do
        (_,typeCase,context,_,symTab) <- get     
        typeTerm <- genNewVar
        typeLeft <- genNewVar
        typeRight <- genNewVar
        termEqns <- genEquationsList [term] [typeTerm]
        pattEqns <- getPattListCase patttermphrs (typeLeft,typeRight)
        let 
          leftEqn = TSimp (TypeVarInt typeLeft,TypeVarInt typeTerm)
          rightEqn= TSimp (TypeVarInt typeCase,TypeVarInt typeRight)
          finEqn  = TQuant ([],[typeTerm,typeRight,typeLeft])
                           ([leftEqn,rightEqn] ++ (termEqns++ pattEqns))
        return [finEqn]


getPattListCase :: [PatternTermPhr] -> (TypeThing,TypeThing) ->
                   ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
                   [TypeEqn]
getPattListCase pattTerms (typeLeft,typeRight) = do 
            (_,_,context,_,symTab) <- get
            totEqn <- helperPattTermCase pattTerms (typeLeft,typeRight)
            return totEqn


helperPattTermCase :: [PatternTermPhr] -> (TypeThing,TypeThing) -> 
                      ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                      [TypeEqn]
helperPattTermCase [] _ 
        = return []
helperPattTermCase (p:ps) (typeinp,typeout) = do 
        pEqns  <- genEqnsPattTermCaseType p [typeinp,typeout]
        psEqns <- helperPattTermCase ps (typeinp,typeout)
        return $ (pEqns++psEqns)

genEqnsPattTermCaseType :: PatternTermPhr -> [TypeThing] -> 
                           ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
                           [TypeEqn]
genEqnsPattTermCaseType (patt:[],eithTerm) [varLeft,varRight] = do 
        (_,_,context,_,symTab) <- get  
        pattEqns <- genPattEquationsList [patt] [varLeft]
        case eithTerm of
            Left term -> do 
                termEqns <- genEquationsList [term] [varRight]
                modify $ \(n,tt,c,chC,st) -> (n,tt,context,chC,symTab) 
                let 
                  totEqns = pattEqns ++ termEqns
                  cEqns   = combineEqns totEqns
                return cEqns

            Right termPairList -> do
                -- check whether termL is a boolean or not
                guardEqns <- handleGuarded pattEqns varRight termPairList
                modify $ \(n,tt,c,chC,st) -> (n,tt,context,chC,symTab)
                return guardEqns    

handleGuarded :: [TypeEqn] -> TypeThing -> [(Term,Term)] -> 
                 ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                 [TypeEqn]
handleGuarded pattEqns varRight termPairList = do 
        handleGuardedHelper pattEqns varRight termPairList  


handleGuardedHelper :: [TypeEqn] -> TypeThing -> [(Term,Term)] -> 
                       ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
                       [TypeEqn]
handleGuardedHelper _ _ [] 
        = return []
handleGuardedHelper pattEqns varRight ((lTerm,rTerm):rest) = do
        newVarL <- genNewVar
        newVarR <- genNewVar

        lEqns <- genEquationsList [lTerm] [newVarL] 
        rEqns <- genEquationsList [rTerm] [newVarR] 
        let 
           -- connecting equation to other guard branches
           tlPosn= getTermPosn lTerm
           qEqn1 = TSimp (TypeVarInt newVarR,TypeVarInt varRight)
           qEqn2 = TSimp (TypeVarInt newVarL,TypeDataType ("Bool",[],tlPosn))
           connEqns = TQuant ([],[newVarL,newVarR]) [qEqn2,qEqn1]
           combEqns = combineEqns (pattEqns ++ (connEqns:(lEqns ++ rEqns)))
        remEqns <- handleGuardedHelper pattEqns varRight rest       
        return (combEqns ++ remEqns)  

-- ===================================================================================
-- ===================================================================================
fun_Fold :: (Term,[FoldPattern],PosnPair) ->
            ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
fun_Fold (term,foldPatts,posn) = do
        (_,_,context,_,symTab) <- get     
        handle_FoldPatts foldPatts term 
      

getFoldName :: FoldPattern -> (Name,PosnPair)
getFoldName (name,_,_,pn) = (name,pn)

handle_FoldPatts :: [FoldPattern] -> Term ->
                    ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
handle_FoldPatts foldPatts term = do 
        (_,typeFold,context,_,symTab) <- get 
        let 
          nmPnPairs = map getFoldName foldPatts
        funPairs <- getAllFoldFuns nmPnPairs
        let 
          dataFun = (snd.head) funPairs
          funs    = (map fst funPairs) ++ [dataFun]              
        (allUVars,allFuns,outType) <- renameFunsTypes funs 
        foldEqns  <- hndl_FPatt_main foldPatts (init allFuns)
        termVar   <- genNewVar
        termEqns1 <- genEquationsList [term] [termVar]
        let
          outEqn    = TSimp  (TypeVarInt typeFold,outType) 
          termEqns2 = TSimp  (TypeVarInt termVar,last allFuns)
          finEqn    = TQuant ([],termVar:allUVars)
                             ([outEqn,termEqns2] ++ termEqns1 ++ foldEqns)
        modify $ \(m,tt,c,chC,st) -> (m,typeFold,context,chC,st)
        return [finEqn]
        
getAllFoldFuns :: [(Name,PosnPair)] -> 
                  ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                                   [(FunType,FunType)]
getAllFoldFuns [] = return []
getAllFoldFuns (np:nps) = do 
        (_,_,context,_,symTab) <- get  
        case lookup_ST (Val_Cons np) symTab of 
            Left emsg -> do 
                throwError emsg 
            Right valRet -> do 
                let 
                  ValRet_Cons (_,_,pair,_) = valRet
                pairs <- getAllFoldFuns nps 
                return (pair:pairs) 


hndl_FPatt_main :: [FoldPattern] -> [Type] -> 
                   ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
hndl_FPatt_main [] [] = return []     
hndl_FPatt_main (p:ps) (ft:fts) = do
        teqn  <- handle_FoldPatt p ft 
        teqns <- hndl_FPatt_main ps fts 
        return $ teqn ++ teqns



handle_FoldPatt :: FoldPattern -> Type -> 
                   ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
handle_FoldPatt (name,patts,term,posn) foldT  = do 
        (_,typeFold,context,_,symTab) <- get
        pattVars <- genNewVarList (length patts)
        pattEqns <- genPattEquationsList patts pattVars
        termVar  <- genNewVar
        termEqns <- genEquationsList [term] [termVar]
        let 
          (ins,out)= getOutFnType foldT 
          inEqns   = zipWith (\x y -> TSimp (TypeVarInt y,x)) ins pattVars 
          outEqn   = TSimp (TypeVarInt termVar,out)
          finEqn   = TQuant ([],termVar:pattVars) 
                           (outEqn:(inEqns ++ pattEqns++termEqns))
        modify $ \(n,tt,c,chC,st) -> (n,typeFold,context,chC,symTab)
        return [finEqn]


-- check whether all constructors are present 
-- first is the list of all constructor in the fold , second is the list of all constructors
checkConses :: [(Name,PosnPair)] -> [Name] -> Name -> SymbolTable -> Either ErrorMsg Bool 
checkConses foldConsPosn allConses datatype symTab = do 
        let 
           foldConses
                   = map fst foldConsPosn
           -- absent conses
           absCons = allConses \\ foldConses
           -- duplicate conses
           dupCons = allConses \\ (nub allConses)
           -- extra conses
           extCons = foldConses \\ allConses
        case extCons == [] of 
            True  -> do 
                case absCons == [] of
                    True  ->   
                        case dupCons == [] of 
                            True  ->
                                return True
                            False -> do 
                                let 
                                  dupConsPosn
                                       = filter (\(x,p) -> elem x dupCons) foldConsPosn
                                  emsg = "Following constructors have been used more than once in" ++
                                         "a fold over  data type <<" 
                                          ++ show datatype ++ ">>.\n" ++ 
                                          (intercalate "\n" (map printConsDest dupConsPosn))
                                Left emsg 

                    False -> do 
                        let 
                          absConsPosn
                               = filter (\(x,p) -> elem x absCons) foldConsPosn
                          emsg = "Following constructors missing for data <<" 
                                 ++ show datatype ++ ">>.\n" ++ 
                                 (intercalate "\n" (map printConsDest absConsPosn))
                        Left emsg                           
            False -> do 
                let 
                  extConsPosn
                       = filter (\(x,p) -> elem x extCons) foldConsPosn
                  emsg = "Extraneous constructors found.Following constructors don't belong to data <<" 
                         ++ show datatype ++ ">>.\n" ++
                          (intercalate "\n" (map printConsDest extConsPosn))
                Left emsg 




-- ===================================================================================
-- ===================================================================================
fun_Unfold ::(Term,FoldPattern,PosnPair) ->
             ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
             [TypeEqn]
fun_Unfold (term,foldPatt,posn) = undefined      


-- ===================================================================================
-- ===================================================================================
fun_Cons ::(Name,[Term],PosnPair) ->
           ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
           [TypeEqn]
fun_Cons (consName,terms,posn) = do 
        (_,typeCons,context,_,symTab) <- get 
        let 
          eithVal = lookup_ST (Val_Cons (consName,posn)) symTab           
        case eithVal of 
            Right retVal -> do 
                let 
                  ValRet_Cons ((datName,allConses),ftype,_,nargs)
                           = retVal 
                case nargs == length terms of
                    True  -> do 
                        renFunType <- renameFunType ftype 
                        newVars    <- genNewVarList nargs
                        termEqns   <- genEquationsList terms newVars
                        let 
                          (univVars,itypes,otype,sposn)
                                  = stripFunType renFunType posn 1     
                          outEqn  = TSimp (TypeVarInt typeCons,otype)
                          inEqns  = zipWith (\x y -> TSimp (TypeVarInt y,x)) 
                                            itypes 
                                            newVars

                          finEqns = TQuant ([],univVars++newVars)
                                           (outEqn:(inEqns ++ termEqns))
                        return [finEqns]
                    False -> do 
                        let
                          emsg 
                            = "Constructor/Destructor <<" ++ show consName ++ ">> " ++ 
                               printPosn posn ++
                              "called with incorrect number of arguments.Expected " 
                               ++ show nargs ++ " ,got " ++ show (length terms) 
                        throwError emsg                                     
                
            Left emsg -> 
                throwError emsg  
-- ===================================================================================
-- ===================================================================================
fun_Dest :: (Name,[Term],PosnPair) ->
            ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
fun_Dest (name,terms,posn) = do 
        (_,typeDest,context,_,symTab) <- get
        let 
          eithVal = lookup_ST (Val_Dest (name,posn)) symTab
        case eithVal of 
            Left emsg ->
                throwError emsg 
            Right retVal -> do 
                let 
                  numTerm = length terms  
                  ValRet_Dest ((cdatName,allDests),funtype,nargs)
                          = retVal 
                case numTerm == nargs  of 
                    True  -> do 
                        renFunType <- renameFunType funtype 
                        termVars   <- genNewVarList nargs
                        termEqns   <- genEquationsList terms termVars
                        let 
                          IntFType (evars,fType)
                                 = renFunType 
                          TypeFun(ins,out,tposn)
                                 = fType 
                          inEqns = zipWith (\x y ->TSimp (TypeVarInt x,y)) termVars ins                                                   
                          outEqn = TSimp (TypeVarInt typeDest,out) 
                          finEqn = TQuant ([],evars++termVars) (outEqn:(inEqns ++ termEqns)) 
                        return [finEqn]                                              

                    False -> do 
                        let 
                          emsg 
                            = "Destructor <<" ++ show name ++ ">> has incorrect number of arguments.\n"
                              ++ "Expected " ++ show nargs ++ ", got " ++ show numTerm
                        throwError emsg 

-- ===================================================================================
-- ===================================================================================
fun_Prod ::([Term],PosnPair) -> 
            ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
fun_Prod (terms,posn) = do 
        (_,typeProd,context,_,symTab) <- get
        termVars  <- genNewVarList (length terms)
        prodEqns  <- genEquationsList terms termVars
        let 
          tSimp  = TSimp (TypeVarInt typeProd,TypeProd (map TypeVarInt termVars,posn))
          finEqn = TQuant ([],termVars) (tSimp:prodEqns)
        modify $ \(n,tt,c,chC,st) -> (n,tt,context,chC,symTab)
        return [finEqn]

-- ===================================================================================
-- ===================================================================================
fun_Default ::PosnPair -> 
              ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
fun_Default posn = do 
        (_,typeDef,context,_,symTab) <- get 
        let 
          eqn = TSimp (TypeVarInt typeDef,TypeDataType ("Bool",[],posn))
        return [eqn]   


-- ============================================================================
-- ============================================================================
-- ============================================================================
-- ============================================================================


{- This function is tailor made for the mutual case -}
genFunDefnListEqns :: [Defn] -> [TypeThing] -> 
                      ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) [TypeEqn]
genFunDefnListEqns [] [] 
        = return  []

genFunDefnListEqns (d:ds) (tthing:trest)  = do 
       (_,_,context,_,symTab) <- get 
       modify $ \(n,tt,c,chC,st) -> (n,tthing,c,chC,st)
       dEqns  <- genFunDefnEqns d "mut"  
       modify $ \(n,tt,c,chC,st) -> (n,tt,context,chC,st)
       dsEqns <- genFunDefnListEqns ds trest 
       return $ combineEqns (dEqns++dsEqns)

{-This is the function for the mutual case-}
takeCareofFunDefns :: [Defn] -> 
                   ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                                    ([Defn],Log,[TypeEqn])
takeCareofFunDefns []    = do 
         return ([],[],[])

takeCareofFunDefns defns = do
        (startNum,_,_,_,_) <- get  
        typeThings <- genNewVarList (length defns)
        skelVars   <- assignSkelTypes defns
        finEqns    <- genFunDefnListEqns defns typeThings         
        let 
          solEqn  = solveEqns finEqns
        case solEqn of
            Left errormsg -> do 
                throwError $ "Error in mutual functions \n\n" ++
                       intercalate "," (map show (getAllFunNames defns))
                       ++"\n" ++ errormsg

            Right logpack -> do 
                 let 
                    (log,package)
                            = logpack 
                    (fvars,uvars,evars,subsList)
                            = package    
                 case mkNewDefnsMut defns startNum subsList [] of 
                     Left errormsg  ->
                         throwError $ errormsg ++ "\n" ++ show package
                     Right finDefns ->
                         return (finDefns,log,finEqns)                 
                 

mkNewDefnsMut :: [Defn] -> Int -> SubstList -> [Defn] ->
                 Either ErrorMsg [Defn]
mkNewDefnsMut [] _ _ finDefns = do 
      return (reverse finDefns)
mkNewDefnsMut ((FunctionDefn (fn,ftype,fnlist,pn)):ds) num sList shDefn = do 
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
                        newDefn  = FunctionDefn (fn,fnStrType,fnlist,pn)
                      mkNewDefnsMut ds (num+1) sList (newDefn:shDefn)
          Nothing -> do 
             let
               oDefn = FunctionDefn (fn,ftype,fnlist,pn)
             mkNewDefnsMut ds (num+1) sList (oDefn:shDefn)


takeCareofFunDefn :: Defn ->
                  ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                                   ([Defn],Log,[TypeEqn])
takeCareofFunDefn defn@(FunctionDefn (fname,mfunType,pattTermList,posn)) = do
    funEqns <- genFunDefnEqns defn "norm" 
    let 
      solEqn = solveEqns funEqns
    case mfunType of
        -- no type was given
        NoType ->
          case solEqn of
            Left errormsg -> do 
                let
                   emsg = ["Type error in function <<",
                           show fname, ">> defined ",
                           printPosn posn, "\n", errormsg 
                          ] 
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
                          "Error renaming function name " ++ show fname,
                          iemsg,intercalate "\n" log,
                          prettyStyle zigStyle funEqns
                         ]  

                   Right fnStrType -> do 
                       let 
                         newDefn = FunctionDefn 
                                      ( fname,fnStrType,
                                        pattTermList,posn
                                      )
                       modify $ \(n,tt,c,chC,st) -> (1,0,[],chC,st)
                       return ([newDefn],log,funEqns)

        otherwise -> 
            case solEqn of
              Left errormsg -> do 
                  let
                    emsg = "\n" ++ "Type Mistmatch : In function <<" ++
                            show fname ++ ">> defined" ++ printPosn posn
                            ++ "\n\n" ++ errormsg ++ "\n" 
                    errDefn 
                         = FunctionDefn (fname,NoType,pattTermList,posn)
                  errEqns <- genFunDefnEqns errDefn "norm" 
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
                                   emsg,"\n",
                                   "Expected Type :: " ++ show fnStrType,"\n",
                                   "Given Type    :: " ++ show mfunType
                                  ]      

              Right (log,pack) -> do  
                   return ([defn],log,funEqns)

-- ===============================================================================
-- ===============================================================================

genFunDefnEqns :: Defn -> String -> 
                  ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
                  [TypeEqn]
genFunDefnEqns defn funkind = do 
        (_,_,context,_,symTab) <- get
        let 
          FunctionDefn (fname,mfunType,pattTermList,posn) = defn 
        newFType <- renameFunType mfunType
        case funkind == "norm" of 
            True  -> do 
                assignSkelType defn 
                funEqns  <- genPattTermListFunc 
                               pattTermList 
                               (fname,posn,newFType)
                modify $ \(n,tt,c,chC,st) -> (n,tt,context,chC,st)
                return funEqns

            False -> do 
                funEqns  <- genPattTermListFunc 
                               pattTermList 
                               (fname,posn,newFType)
                modify $ \(n,tt,c,chC,st) -> (n,tt,context,chC,st)
                return funEqns

genPattTermListFunc :: [(PatternTermPhr,PosnPair)] -> 
                       (FuncName,PosnPair,FunType) ->
                       ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
                       [TypeEqn]
genPattTermListFunc pattTerms (fname,fposn,fType) = do 
        (_,typeFunDefn,context,_,symTab) <- get
        let 
          funVal  = Val_Fun (fname,fposn)
          eithVal = lookup_ST funVal symTab  
        case eithVal of
            Left  emsg -> do 
                throwError emsg 
            Right valRet -> do 
                let 
                  ValRet_Fun (skelFunType,numins)
                        = valRet
                  IntFType (pars,funType)
                        = skelFunType  
                  TypeFun (skelIns,skelOut,skelPosn) 
                        = funType     
                  nargs = numins +1    
                  lowestEqn 
                        = TSimp (TypeVarInt typeFunDefn,funType)
                  skelVars 
                        = concat $ map freeVars (skelOut:skelIns)
                pattTermEqns <- helperEqnsPattern 
                                     (skelVars,nargs)
                                     pattTerms 
                let 
                  totEqn = TQuant ([],skelVars)
                                  (lowestEqn:pattTermEqns)  
                case fType of
                    IntFType _ -> do 
                        let
                          (intFUVars,inTypes,outType,sposn)
                              = stripFunType fType fposn 0
                          newFunType
                              = TypeFun (inTypes,outType,sposn) 
                          givenEqn 
                              = TQuant (intFUVars,[typeFunDefn])
                                       [TSimp (TypeVarInt typeFunDefn,newFunType)]
                          newTotEqn
                              = combineEqns [givenEqn,totEqn]                 
                        return newTotEqn
                      
                    otherwise ->
                        return [totEqn]


genEqnsPattTermFunType :: (PatternTermPhr,PosnPair) -> ([Int],Int) -> 
                   ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
                   [TypeEqn]
genEqnsPattTermFunType ((patts,eithTerm),posn) (varsLeft,varRight) = do 
        (_,_,context,_,symTab) <- get  
        pattEqns <- genPattEquationsList patts varsLeft      
        case eithTerm of
            Left term -> do 
                termEqns <- genEquationsList [term] [varRight]
                modify $ \(n,tt,c,chC,st) -> (n,tt,context,chC,symTab) 
                let 
                  totEqns = pattEqns ++ termEqns
                return $ combineEqns totEqns

            Right termPairList -> do
                -- this will be the final equation to return
                guardEqns <- handleGuarded pattEqns varRight termPairList
                modify $ \(n,tt,c,chC,st) -> (n,tt,context,chC,symTab)
                return guardEqns      

helperEqnsPattern :: ([Int],Int) -> [(PatternTermPhr,PosnPair)] -> 
                     ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable)) 
                     [TypeEqn]

helperEqnsPattern _ []  
        = return []

helperEqnsPattern (skelVars,nVars) (fstPattTerm:restPatts) = do 
        pattVars <- genNewVarList nVars  
        let 
          eqnsEquality
                   = zipWith (\x y -> TSimp (TypeVarInt y,TypeVarInt x)) skelVars pattVars
        eqnsNonEquality <- genEqnsPattTermFunType fstPattTerm (tail pattVars,head pattVars)
        let 
          fstPattEqns = TQuant ([],pattVars) (eqnsEquality ++ eqnsNonEquality)
        remPattEqns <- helperEqnsPattern (skelVars,nVars) restPatts 
        return $ (fstPattEqns:remPattEqns)


-- ==================================================================================
-- ==================================================================================
-- ==================================================================================

-- This function updates the symbol table with a skeleton type, returns the skeleton variables
-- used in the skeleton type along with total number of variable that each line in function
-- i.e pattern and term on the right would have to generate.
assignSkelType :: Defn ->
                  ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
                  [TypeThing]
                                   
assignSkelType defn 
        = case defn of 
            FunctionDefn (fname,fType,pattTerms,fPosn) -> do 
                (_,typeFunDefn,context,_,symTab) <- get
                let 
                  fstPTerm = head pattTerms
                  ((fstPatts,fstTerm),fstPosn)
                           = fstPTerm
                  nargs    = length fstPatts  
                skelVars <- genNewVarList (nargs +1)
                let 
                  outSkel:inSkel 
                          =  skelVars
                  skelFun = TypeFun 
                                  ( map TypeVarInt inSkel,
                                    TypeVarInt outSkel,
                                    fstPosn
                                  ) 
                  fstFunType
                          = IntFType ([],skelFun)
                  funDefn = FunctionDefn (fname,fstFunType,pattTerms,fPosn)
                  newST   = insert_ST [funDefn] symTab OldScope
                modify $ \(n,tt,c,chC,st) -> (n,typeFunDefn,context,chC,newST)
                return skelVars

            ProcessDefn (pname,_,pattProc,pn) -> do 
                (_,typeProcDefn,context,_,symTab) <- get
                let 
                  (patts,inchs,ochs,_)
                      = pattProc
                pattvars <- genNewVarList (length patts)
                inVars   <- genNewVarList (length inchs)
                outVars  <- genNewVarList (length ochs)
                let 
                  skelProc = ProtProc 
                                 (
                                   map TypeVarInt pattvars,map TypeVarInt inVars,
                                   map TypeVarInt outVars,pn 
                                 )

                  fstProcType
                           = IntFType ([],skelProc)
                  procDefn = ProcessDefn (pname,fstProcType,pattProc,pn)
                  newST    = insert_ST [procDefn] symTab OldScope
                modify $ \(n,tt,c,chC,st) -> (n,typeProcDefn,context,chC,newST)
                return $ pattvars ++ inVars ++ outVars

                 


assignSkelTypes :: [Defn] -> 
                   ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
                   [TypeThing] 
                                    
assignSkelTypes []
        = return []
assignSkelTypes (d:ds) = do 
        sv  <- assignSkelType d 
        svs <- assignSkelTypes ds 
        return $ sv ++ svs
