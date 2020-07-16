{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module MPLPasses.ToGraph where

import Optics 
import Optics.State
import Optics.State.Operators

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST
import MPLPasses.ToGraphErrors 
import MPLPasses.SymbolTable

import MPLPasses.ToGraphTypes
import MPLPasses.ToGraphErrors

import MPLPasses.TieTypeClause
import MPLPasses.TypeClauseSanityErrors
import MPLPasses.Unification
import MPLPasses.InferExprType
import MPLPasses.TieTermUtils
import MPLPasses.TiePattern

import MPLUtil.Data.Tuple.Optics
import MPLUtil.Data.Either.AccumEither

import Data.Maybe
import Control.Monad.RWS
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Bifunctor as Bifunctor
import Control.Arrow

import Data.Map ( Map (..) )
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Except

import Debug.Trace

progInterfaceToGraph :: 
    Prog (DefnI BnfcIdent) ->
    Either ToGraphErrors (Prog (DefnG TaggedBnfcIdent TypeTag))
progInterfaceToGraph (Prog stmts) = Prog <$> (runAccumEither . sequenceA $ res)
  where
    res :: [AccumEither ToGraphErrors (Stmt (DefnG TaggedBnfcIdent TypeTag))]
    res = f defaultToGraphState stmts

    f :: 
        ToGraphState -> 
        [Stmt (DefnI BnfcIdent)] ->
        [AccumEither ToGraphErrors (Stmt (DefnG TaggedBnfcIdent TypeTag))]
    f st [] = []
    f st (stmt:stmts) = case runStateT (stmtIToGraph stmt) st of
        Right ((syms, nstmt), st') -> 
            (liftAEither $ Right nstmt) : f (st' & toGraphSymbolTable %~ (syms++)) stmts
        Left errs -> (liftAEither $ Left errs) : f st stmts

stmtIToGraph ::
    Stmt (DefnI BnfcIdent) -> 
    StateT ToGraphState 
        (Either ToGraphErrors) (SymbolTable, Stmt (DefnG TaggedBnfcIdent TypeTag))
stmtIToGraph (Stmt defs wstmts) = mdo
    (wsyms, wstmts') <- first concat . unzip <$> traverse stmtIToGraph wstmts

    st <- get

    let syms = wsyms ++ st ^. toGraphSymbolTable

    let res = traverse f defs 
        f def = do
            st <- get 
            case runStateT (defIToGraph def) (st & toGraphSymbolTable .~ syms) of
                Right ((symtable, def'), st') -> do
                    uniqueTag .= st' ^. uniqueTag
                    return $ liftAEither $ Right (symtable, def')
                Left errs -> return $ liftAEither $ Left errs
        (res', st') = first (runAccumEither . sequenceA) $ runState res st

    uniqueTag .= st' ^. uniqueTag

    ~(defsyms, defs') <- first (concat . NE.toList) . NE.unzip <$> liftEither res'

    return (defsyms, _Stmt # (defs', wstmts'))

defIToGraph :: 
    DefnI BnfcIdent -> 
    StateT ToGraphState 
        (Either ToGraphErrors) 
        ( SymbolTable
        , DefnG TaggedBnfcIdent TypeTag)
defIToGraph n = case n ^. unDefnI of
    DataDefn n -> do
        let sanity = liftAEither $ typeClauseArgsSanityCheck n 
            phraseto = liftAEither $ phraseToVarsAreStateVar n 
        objectDefnIToGraph _SymSeqClause [sanity, phraseto] n
        
    CodataDefn n -> do
        let sanity = liftAEither $ typeClauseArgsSanityCheck n 
            codatacheck = liftAEither $ codataStateVarOccurenceCheck n 
        objectDefnIToGraph _SymSeqClause [sanity, codatacheck] n

    ProtocolDefn n -> do
        let sanity = liftAEither $ typeClauseArgsSanityCheck n 
            phraseto = liftAEither $ phraseToVarsAreStateVar n 
        objectDefnIToGraph _SymConcClause [sanity, phraseto] n

    CoprotocolDefn n -> do
        let sanity = liftAEither $ 
                    typeClauseArgsSanityCheck n 
            phrasefrom = liftAEither $ 
                    phraseFromVarsAreStateVar n 
        objectDefnIToGraph _SymConcClause [sanity, phrasefrom] n

    FunctionDecDefn n -> do
        n' <- functionDefIToGraph n
        return 
            ( [ ( n' ^. funName % taggedBnfcIdentName
                , _SymEntry # (n' ^. funName % uniqueTag, SymFunDefn n') ) ]
            , FunctionDecDefG n' )
    ProcessDecDefn n -> undefined

---------------------
-- object defn to graph
---------------------
-- errorprism is the prism to construct the error...
-- matchprism is used to match the type from the symbol table...
-- checks is a list of AccumEither so that we can check the validity of the object
-- n is the typeclause
objectDefnIToGraph matchprism checks n = do
    -- get the symtable
    symtable <- use toGraphSymbolTable
    -- get the current fresh tag
    uniquetag <- freshUniqueTag
    let symtable' = mapMaybe f symtable
        -- filters the relevant symbol table entries
        f (str, (SymEntry tag info)) = case info ^? matchprism of
            Just clauseg -> Just (str, _SymEntry # (tag, _SymTypeClause # clauseg ))
            _ -> Nothing
        tie = liftAEither $ 
            makeTypeClauseGraph DataObj (_TieTypeClauseContext # (symtable', uniquetag)) n

    liftEither $ Bifunctor.first (review _MkToGraphErrors )
        $ runAccumEither 
        $ sequenceA checks

    (uniquetag', clausesgraph) <- liftEither 
        $ Bifunctor.first (review _MkToGraphErrors )
        $ runAccumEither tie

    uniqueTag .= uniquetag'

    return (collectClauseGraphSymbolTable clausesgraph, ObjectG clausesgraph)

collectClauseGraphClauses ::
    ClausesGraph TaggedBnfcIdent -> 
    [(String, TypeClauseG TaggedBnfcIdent)]  
collectClauseGraphClauses graph = map f $ NE.toList $ graph ^. clauseGraphSpine
  where
    f = view (typeClauseName % taggedBnfcIdentName ) &&& id

collectClauseGraphPhrases ::
    ClausesGraph TaggedBnfcIdent -> 
    [(String, TypePhraseG TaggedBnfcIdent)]
collectClauseGraphPhrases graph = concatMap f $ NE.toList $ graph ^. clauseGraphSpine
  where
    f graph = map g (graph ^. typeClausePhrases)
    g = view (typePhraseName % taggedBnfcIdentName) &&& id 

collectClauseGraphSymbolTable :: 
    ClausesGraph TaggedBnfcIdent ->
    SymbolTable
collectClauseGraphSymbolTable graph 
    | graph ^. clauseGraphObjectType == DataObj
        || graph ^. clauseGraphObjectType == CodataObj = 
            map (second (review _SymEntry <<< view (typeClauseName % uniqueTag) &&& SymSeqClause)) clauses 
            ++ map (second (review _SymEntry <<< view (typePhraseName % uniqueTag) &&& SymSeqPhrase)) phrases
    | otherwise = 
            map (second (review _SymEntry <<< view (typeClauseName % uniqueTag) &&& SymConcClause)) clauses 
            ++ map (second (review _SymEntry <<< view (typePhraseName % uniqueTag) &&& SymConcPhrase)) phrases
  where
    clauses = collectClauseGraphClauses graph
    phrases = collectClauseGraphPhrases graph

-- function defn to graph....
functionDefIToGraph ::
    FunctionDefnI BnfcIdent ->
    StateT ToGraphState 
        (Either ToGraphErrors) 
        (FunctionDefG TaggedBnfcIdent TypeTag)
functionDefIToGraph (FunctionDefn funident funtype fundefn) = mdo
    funident' <- tagBnfcIdent funident
    ttype <- freshTypeTag

    (bodyg, eqns) <- patternsIAndExprsIToGraph tagmap ttype $ NE.toList fundefn
    pkg <- liftEither $ Bifunctor.first liftToGraphErrors $ solveTypeEq eqns

    let tagmap = packageToTagMap pkg
    -- let TypeSeq (TypeSeqArrF from to) = traceShowId $ (fromJust $ Map.lookup ttype tagmap)

    return $ FunctionDefn funident' (fromJust $ Map.lookup ttype tagmap) (NE.fromList $ bodyg)

exprIToGraph :: 
    -- | type tag map 
    TagTypeMap -> 
    -- | TypeTag for the phrase
    TypeTag -> 
    -- | Expression to convert to a graph
    ExprI BnfcIdent ->
    -- | result
    StateT ToGraphState 
        (Either ToGraphErrors) 
        ( ExprG TaggedBnfcIdent TypeTag 
        , TypeEqns TaggedBnfcIdent TypeTag )
exprIToGraph tagmap ttype expr = 
    f ttype expr
  where 
    -- think carefully about how we will do the built in types...
    -- right now, the implementation is bad...
    {-
    f ttype (EIf eif ethen eelse ()) = do
        ttypeif <- freshTypeTag
        ttypethen <- freshTypeTag
        ttypeelse <- freshTypeTag

        (eifg, eifeqns) <- f ttypeif eif
        (etheng, etheneqns) <- f ttypethen ethen
        (eelseg, eelseeqns) <- f ttypeelse eelse

        return 
            ( EIf eifg etheng eelseg $ fromJust $ Map.lookup ttype tagmap
            , TypeEqnsExist 
                [ttypeif, ttypethen, ttypeelse] $ 
                [ TypeEqnsEq (TypeVar ttypeif, TypeSeq TypeBoolF)
                , TypeEqnsEq (TypeVar ttype, TypeVar ttypethen)
                , TypeEqnsEq (TypeVar ttype, TypeVar ttypeelse)
                , eifeqns
                , etheneqns
                , eelseeqns                
                ]
            )
    f ttype (ELet letdef expr ()) = do
        -- TODO figure this thing out..
        error "LET is TODO"
    f ttype (EOp larg op rarg ()) = 
        -- TODO
        error "operators not implemented in type checking yet"
        -}
    f ttype (EConstructorDestructor ident () args ()) = do
        (tag, phraseg) <- lookupSeqPhrase ident
        case phraseg ^. phraseGObjType of
            DataObj -> ctsrunner tag phraseg
            CodataObj -> dtsrunner tag phraseg
            _ -> error "internal error -- looked up a sequential phrase but got a concurrent phrase"
      where
        ctsrunner tag phraseg = do  
            let expectedarity = length (phraseg ^. typePhraseFrom)
                actualarity = length args
            unless
                (expectedarity == actualarity)
                (throwError $ liftToGraphErrors 
                    (_ArityMismatch # (ident, expectedarity, actualarity)))

            --  query the phrase substitutions from the graph
            (clausetype, ttypeargs, clausesubstitutions) <- 
                    clauseSubstitutions (phraseg ^. typePhraseContext % phraseParent)

            -- fresh type vars for the constructor args
            ttypectsargs <- traverse (const freshTypeTag) args

            (argsexprgs, argseqns) <- unwrappedExprsIToGraph tagmap 
                $ zip ttypectsargs args

            let typeeqs = TypeEqnsExist (ttypectsargs ++ ttypeargs) $
                    [ TypeEqnsEq ( _TypeVar # ttype
                        , clausetype ) ] 
                    ++ zipWith g ttypectsargs (phraseg ^. typePhraseFrom)
                    ++ argseqns
                g typetag ctsargtype = TypeEqnsEq
                    ( TypeVar typetag
                    , fromJust $ substitutesTypeGToTypeGTypeTag 
                        clausesubstitutions ctsargtype )
                expr' = EConstructorDestructor 
                    (_TaggedBnfcIdent # (ident, tag)) 
                    (ConstructorDestructorKnot phraseg)
                    -- TODO, we can get more type safety here!
                    -- we know that this will always be a 
                    -- constructor / destructor
                    argsexprgs 
                    $ fromJust $ Map.lookup ttype tagmap
            return ( expr', typeeqs)

        dtsrunner tag phraseg = do
            -- TODO
            undefined

    f ttype ecase@(ECase caseon cases ()) = do

        -- converting the case on to a graph...
        ttypecaseon <- freshTypeTag
        (caseong, caseongeqns) <- exprIToGraph tagmap ttypecaseon caseon

        -- converting the case phrases to a graph..
        ttypetaggedcases <- traverse 
            (\(patt, exp) -> (,) 
                <$> ((,patt) <$> freshTypeTag) 
                <*> ((,exp) <$> freshTypeTag)) 
            cases

        (pattsgexprsg, eqns) <- unzip <$> traverse 
            (\((ttypepatt, patt), (ttypeexpr, expr)) -> do
                ((sym, patteqns), pattsg) <- 
                    patternIToGraph tagmap ttypepatt patt
                st <- get
                ((exprg, expreqn), st') <- liftEither $ runStateT 
                    (exprIToGraph tagmap ttype expr) 
                    (st & toGraphSymbolTable %~ (sym++))

                uniqueTag .= st' ^. uniqueTag
                return ((pattsg, exprg), expreqn:patteqns) )
                (NE.toList ttypetaggedcases)

        let expr' = ECase caseong (NE.fromList pattsgexprsg) $ fromJust $ Map.lookup ttype tagmap
            -- patt typing info
            ttypepatts      = NE.toList $ fmap (fst . fst) ttypetaggedcases
            ttypeexprs      = NE.toList $ fmap (fst . snd) ttypetaggedcases
            typeeqs = TypeEqnsExist 
                (ttypecaseon : ttypepatts ++ ttypeexprs)
                $ map (TypeEqnsEq . (TypeVar ttypecaseon,) . TypeVar) ttypepatts
                    -- the type being cased on must be the same as the patterns 
                ++ map (TypeEqnsEq . (TypeVar ttype,) . TypeVar) ttypeexprs
                    -- the type of the expression is the same as the result of the case
                ++ concat eqns
                -- collecting the equations..
        return (expr', typeeqs)


patternsIAndExprsIToGraph :: 
    TagTypeMap -> 
    TypeTag -> 
    [([PatternI BnfcIdent], ExprI BnfcIdent)] ->
    StateT ToGraphState 
        (Either ToGraphErrors) 
        ( [([PatternG TaggedBnfcIdent TypeTag], ExprG TaggedBnfcIdent TypeTag)] 
        , TypeEqns TaggedBnfcIdent TypeTag )
patternsIAndExprsIToGraph tagmap ttype pattsandexps = do
    ttypepattsandexps <- traverse (\n -> (,n) <$> freshTypeTag ) pattsandexps
    (pattsgexpsg, eqns) <- unzip <$> 
        traverse (uncurry (patternIAndExprIToGraph tagmap)) ttypepattsandexps

    let ttypephrases = map fst ttypepattsandexps
        typeeqns = TypeEqnsExist ttypephrases $
            map (TypeEqnsEq . (TypeVar ttype,) . TypeVar) ttypephrases
            ++ eqns
    return (pattsgexpsg, typeeqns)

patternIAndExprIToGraph ::
    TagTypeMap -> 
    -- | Type Tag
    TypeTag -> 
    -- | Patterns and expr
    ([PatternI BnfcIdent], ExprI BnfcIdent) ->
    StateT ToGraphState 
        (Either ToGraphErrors) 
        ( ([PatternG TaggedBnfcIdent TypeTag], ExprG TaggedBnfcIdent TypeTag)
        , TypeEqns TaggedBnfcIdent TypeTag )
patternIAndExprIToGraph tagmap ttype (patts, expr) = do
    -- tag the patterns expressions with a type..
    (ttypepatts, (ttypeexpr, expr)) <- do
            ttypepatts <- traverse (const freshTypeTag) patts
            ttypeexpr <- freshTypeTag
            return (zip ttypepatts patts, (ttypeexpr, expr))

    -- get the symbol table, equations, and graph pattern from the type patterns
    ((syms, patteqns), pattsg) <- unwrappedPatternsIToGraph tagmap ttypepatts

    -- get the current state
    st <- get

    -- with that state, append the extended context from the patterns
    -- and convert the expression to the graph
    ((exprg, expreqns), st') <- liftEither $ runStateT 
        (exprIToGraph tagmap ttypeexpr expr) (st & toGraphSymbolTable %~ (syms++))

    -- update the unique tag generation  (need tags to be unique)
    -- but note that this does not alter the context since the context
    -- of each expression is independent
    toGraphUniqueTagGen .= st' ^. toGraphUniqueTagGen

    let ttypeeqn = TypeEqnsExist (ttypeexpr : map fst ttypepatts) $ 
                [ TypeEqnsEq 
                    ( TypeVar ttype
                    , TypeSeq $ TypeSeqArrF 
                        (map (TypeVar . fst) ttypepatts) 
                        (TypeVar ttypeexpr) 
                    ) ]
                ++ patteqns ++ [expreqns]

    return ((pattsg, exprg), ttypeeqn)


exprsIToGraph :: 
    -- | type tag map 
    TagTypeMap -> 
    -- | Expression to convert to a graph
    [(TypeTag, ExprI BnfcIdent)] ->
    -- | result
    StateT ToGraphState 
        (Either ToGraphErrors) 
        [( ExprG TaggedBnfcIdent TypeTag 
        , TypeEqns TaggedBnfcIdent TypeTag )]
exprsIToGraph tagmap ttypesexprs = traverse (uncurry $ exprIToGraph tagmap) ttypesexprs 

unwrappedExprsIToGraph ::
    -- | type tag map 
    TagTypeMap -> 
    -- | Expression to convert to a graph
    [(TypeTag, ExprI BnfcIdent)] ->
    -- | result
    StateT ToGraphState 
        (Either ToGraphErrors) 
        ( [ExprG TaggedBnfcIdent TypeTag]
        , [TypeEqns TaggedBnfcIdent TypeTag])
unwrappedExprsIToGraph tagmap ttypesexprs = unzip <$> exprsIToGraph tagmap ttypesexprs
