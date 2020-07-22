{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
module MPLPasses.TieDefns where

import Optics 
import Optics.State
import Optics.State.Operators

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST

import MPLPasses.GraphGenCore
import MPLPasses.TieTypeClause
import MPLPasses.TypeClauseSanityErrors
import MPLPasses.Unification
import MPLPasses.InferExprType
import MPLPasses.TiePattern
import MPLPasses.SymbolTable
import MPLPasses.TieDefnsTypes
import MPLPasses.TieDefnsErrors 
import MPLPasses.TieDefnsUtils


import MPLUtil.Data.Tuple.Optics
import MPLUtil.Data.Either.AccumEither

import Data.Maybe
import Data.List
import Data.Bool
import Data.Tuple
import Control.Monad.RWS
import Control.Monad.Writer
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Bifunctor as Bifunctor
import Control.Arrow
import Data.Either

import Data.Map ( Map (..) )
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Applicative

import Debug.Trace 

newtype TieDefnsT m a = 
    TieDefnsT { unTieDefnsT :: RWST 
            [DefnG TaggedBnfcIdent TypeTag] 
            [DefnG TaggedBnfcIdent TypeTag] 
            SymbolTable 
            m a }
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadState SymbolTable
    , MonadReader [DefnG TaggedBnfcIdent TypeTag]
    , MonadWriter [DefnG TaggedBnfcIdent TypeTag] 
    , MonadRWS [DefnG TaggedBnfcIdent TypeTag] [DefnG TaggedBnfcIdent TypeTag] SymbolTable  
    , MonadTrans 
    )

type TieDefns a = TieDefnsT GraphGenCore a

newtype TieFunT m a =
    TieFunT { 
        unTieFunT :: ReaderT TieExprEnv (StateT SymbolTable m) a
    }
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadState SymbolTable
    , MonadReader TieExprEnv)
instance MonadTrans TieFunT where
    lift = TieFunT . lift . lift 

data ExprTypeTags = ExprTypeTags {
    _exprTtype :: TypeTag
    , _exprTtypeInternal :: TypeTag
}

data TieExprEnv = TieExprEnv {
    _tieExprEnvTypeTags :: ExprTypeTags
    , _tieExprEnvTagTypeMap :: TagTypeMap
}

$(makeLenses ''TieExprEnv)
$(makePrisms ''TieExprEnv)
$(makeClassy ''ExprTypeTags)
$(makePrisms ''ExprTypeTags)

freshExprTypeTags = ExprTypeTags <$> freshTypeTag <*> freshTypeTag

instance HasExprTypeTags TieExprEnv where
    exprTypeTags = tieExprEnvTypeTags

type TieFun a = TieFunT GraphGenCore a

runTieDefnsT :: 
    SymbolTable ->
    TieDefns a -> 
    GraphGenCore (a, SymbolTable, [DefnG TaggedBnfcIdent TypeTag])
runTieDefnsT st (TieDefnsT m) = runRWST m [] st

progInterfaceToGraph :: 
    (SymbolTable, GraphGenCoreEnv, GraphGenCoreState) ->
    Prog (DefnI BnfcIdent) ->
    Either [TieDefnsError] (Prog (DefnG TaggedBnfcIdent TypeTag))
progInterfaceToGraph initstate (Prog stmts) = Prog <$> res 
  where
    res :: Either [TieDefnsError] [Stmt (DefnG TaggedBnfcIdent TypeTag)]
    res = accumEithers $ f initstate stmts

    f :: 
        (SymbolTable, GraphGenCoreEnv, GraphGenCoreState) -> 
        [Stmt (DefnI BnfcIdent)] -> 
        [Either [TieDefnsError] (Stmt (DefnG TaggedBnfcIdent TypeTag))]
    f (sym, env, corest) [] = []
    f (sym, env, corest) (stmt:rst) = 
         stmt' : f (sym', env, corest') rst
      where
        (stmtg, corest', errlg) = 
            runRWS (unGraphGenCore $ runReaderT (stmtIToGraph stmt) sym) 
                env
                corest
        stmt' = bool (Left errlg) (Right stmtg) (null errlg)
        sym' = either (const []) collectSymEntries stmt' ++  sym

stmtIToGraph ::
    Stmt (DefnI BnfcIdent) -> 
    ReaderT SymbolTable GraphGenCore (Stmt (DefnG TaggedBnfcIdent TypeTag))
stmtIToGraph (Stmt defs wstmts) = do
    -- compute new where bindings first ..
    wstmts' <- traverse stmtIToGraph wstmts
    let wsyms = mconcat . map collectSymEntries $ wstmts'

    symtab <- ask
    tag <- freshUniqueTag
    -- compute the definitions graph
    ((), st, defs') <- lift $ runTieDefnsT (wsyms++symtab) 
            (defnsToGraph (NE.toList defs)) 

    return $ Stmt (NE.fromList defs') wstmts'

defnsToGraph :: 
    [DefnI BnfcIdent] ->
    TieDefns ()
defnsToGraph [] = return ()
defnsToGraph (a:as) = case a ^. unDefnI of 
    DataDefn ival -> mdo
        objectDefIToGraph 
            ival
            [ lift $ typeClauseArgsSanityCheck ival , lift $ phraseToVarsAreStateVarCheck ival ]
            (querySymbolTableSequentialClauses symtable)
            DataObj

        defnsToGraph as
        symtable <- guse equality
        return ()

    CodataDefn ival -> mdo
        objectDefIToGraph 
            ival
            [ lift $ typeClauseArgsSanityCheck ival, lift $ codataStateVarOccurenceCheck ival ]
            (querySymbolTableSequentialClauses symtable)
            CodataObj

        defnsToGraph as
        symtable <- guse equality
        return ()

    ProtocolDefn n -> mdo
        undefined

        -- lift $ typeClauseArgsSanityCheck n 
        -- lift $ phraseToVarsAreStateVarCheck n

    CoprotocolDefn n -> mdo
        undefined
        -- lift $ typeClauseArgsSanityCheck n 
        -- lift $ phraseFromVarsAreStateVarCheck n 
    FunctionDecDefn fun -> mdo
        -- put on symbol table first...
        equality %= ((collectSymEntries fun')++)

        ~fun' <- lift $ FunctionDecDefG <$> functionDefIToGraph symtable fun

        tell [fun']

        defnsToGraph as

        symtable <- guse equality

        return ()

    ProcessDecDefn n -> undefined

objectDefIToGraph ival checks symtable objtype = do
    sequenceA checks
    ~clauseg <- lift $ tieTypeClauseGraph symtable objtype ival
    let defn' = ObjectG clauseg :: DefnG TaggedBnfcIdent TypeTag

    equality %= ((collectSymEntries defn')++)

    tell [defn']

    return () 

-- function defn to graph....
functionDefIToGraph ::
    SymbolTable ->
    FunctionDefnI BnfcIdent ->
    GraphGenCore (FunctionDefG TaggedBnfcIdent TypeTag)
functionDefIToGraph symtable ~(FunctionDefn funident funtype fundefn) = mdo
    funident' <- tagBnfcIdent funident

    ttypes <- freshExprTypeTags
    let ttype = ttypes ^. exprTtype
        ttypeinternal = ttypes ^. exprTtypeInternal

    ~(~(bodyg, eqns), st) <- runStateT (
            runReaderT 
            (unTieFunT $ patternsIAndExprsIToGraph $ NE.toList fundefn)
            (_TieExprEnv # (ttypes, tagmap))
            ) symtable

    let tagmap = packageToTagMap pkg' 
        pkg = solveTypeEq eqns
        pkg' = fromRight (error "internal type checking error") pkg
        fun' = FunctionDefn funident' (fromJust $ Map.lookup ttype tagmap) (NE.fromList $ bodyg)
        -- TypeSeq (TypeSeqArrF from to) = traceShowId $ (fromJust $ Map.lookup ttype tagmap)

    tell $ either pure (const []) pkg

    return fun' 


exprIToGraph :: 
    -- | Expression to convert to a graph
    ExprI BnfcIdent ->
    -- | result
    TieFun ( ExprG TaggedBnfcIdent TypeTag, [TypeEqns TaggedBnfcIdent TypeTag] )
exprIToGraph expr = 
    f expr
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
                [ TypeEqnsEq (TypeVar ttypeif [], TypeSeq TypeBoolF)
                , TypeEqnsEq (TypeVar ttype [], TypeVar ttypethen [])
                , TypeEqnsEq (TypeVar ttype [], TypeVar ttypeelse [])
                , eifeqns
                , etheneqns
                , eelseeqns                
                ]
            )
    f ttype (EOp larg op rarg ()) = 
        -- TODO
        error "operators not implemented in type checking yet"
        -}

    f (ELet letdef letexpr ()) = do
        -- TODO figure this thing out..
        symtable <- guse equality 
        ttype <- gview exprTtype
        ttypeinternal <- gview exprTtypeInternal
        tagmap <- gview tieExprEnvTagTypeMap

        -- compiling the let statemtens..
        letdef' <- lift . join . fmap writer . splitGraphGenCore 
            $ runReaderT (traverse stmtIToGraph letdef) symtable 
        let nsyms = concatMap collectSymEntries letdef'

        -- updating the symbol table
        equality %= (nsyms++)

        -- updating the expression
        ttypesletexpr <- lift freshExprTypeTags
        (letexprg, exprgeq) <- local (set tieExprEnvTypeTags ttypesletexpr) $ exprIToGraph letexpr

        let ttypeletexpr = ttypesletexpr ^. exprTtype
            eqns = TypeEqnsExist [ttypeletexpr] $
                [ TypeEqnsEq ( TypeVar ttype []
                             , TypeVar ttypeinternal []) 
                , TypeEqnsEq ( TypeVar ttype [] 
                             , TypeVar ttypeletexpr []) ]
                ++ exprgeq

        return 
            ( ELet letdef' letexprg $ fromJust $ Map.lookup ttypeinternal tagmap
            , [eqns] )

    f (EConstructorDestructor ident () args ()) = do
        -- symtable <- lift $ gview equality
        symtable <- guse equality

        ~(_, ~(SymEntry tag ~(SymPhrase phraseg))) <- lift $ fromJust <$> 
            ( ambiguousLookupCheck 
            =<< querySymbolTableSequentialPhrases 
            <$> querySymbolTableBnfcIdentName ident symtable )
    
        case phraseg ^. phraseGObjType of
            DataObj -> ctsrunner tag phraseg
            CodataObj -> undefined
            -- CodataObj -> dtsrunner tag phraseg
      where
        ctsrunner tag phraseg = do  
            ttype <- gview exprTtype 
            internalttype <- gview exprTtypeInternal 

            tagmap <- gview tieExprEnvTagTypeMap 

            let expectedarity = length (phraseg ^. typePhraseFrom)
                actualarity   = length args
            lift $ tell $ bool 
                    [_ArityMismatch # (ident, expectedarity, actualarity)]
                    []
                    (expectedarity == actualarity)

            --  query the phrase substitutions from the graph
            (clausetype, ttypeargs, clausesubstitutions) <- 
                    lift $ clauseSubstitutions (phraseg ^. typePhraseContext % phraseParent)
            
            -- fresh type vars for the constructor args
            ttypesctsargs <- traverse (const (lift freshExprTypeTags)) args

            (argsexprgs, argseqns) <- unwrappedExprsIToGraph $ zip ttypesctsargs args

            let ttypectsargs = map (view exprTtype) ttypesctsargs
                typeeqs = TypeEqnsExist (ttypectsargs ++ ttypeargs) $
                    [ TypeEqnsEq ( _TypeVar # (ttype, [])
                        , clausetype )
                    , TypeEqnsEq (TypeVar ttype [], TypeVar internalttype [])] 
                    ++ zipWith g ttypectsargs (phraseg ^. typePhraseFrom)
                    ++ argseqns
                g typetag ctsargtype = TypeEqnsEq
                    ( TypeVar typetag []
                    , fromJust $ substitutesTypeGToTypeGTypeTag 
                        clausesubstitutions ctsargtype )
                expr' = EConstructorDestructor 
                    (_TaggedBnfcIdent # (ident, tag)) 
                    (ConstructorDestructorKnot phraseg)
                    -- TODO, we can get more type safety here!
                    -- we know that this will always be a 
                    -- constructor / destructor
                    argsexprgs 
                    $ fromJust $ Map.lookup internalttype tagmap
            return ( expr', [typeeqs] )

        dtsrunner tag phraseg = do
            -- TODO
            undefined

    f (ECase caseon cases ()) = do
        ttype <- gview exprTtype
        ttypeinternal <- gview exprTtypeInternal
        tagmap <- gview tieExprEnvTagTypeMap

        -- converting the case on to a graph...
        -- ttypecaseon <- lift freshTypeTag
        ttypeexprcaseon@(ExprTypeTags ttypecaseon _) <- lift freshExprTypeTags 
        (caseong, caseongeqns) <- local (set tieExprEnvTypeTags ttypeexprcaseon) $ exprIToGraph caseon

        -- converting the case phrases to a graph..
        ttypetaggedcases <- lift $ traverse 
            (\(patt, exp) -> (,) 
                <$> ((,patt) <$> freshExprTypeTags) 
                <*> ((,exp)  <$> freshExprTypeTags)) 
            cases

        (pattsgexprsg, eqns) <- unzip <$> traverse 
            (\((ttypepatt, patt), (ttypeexpr, expr)) -> do
                symtable <- guse equality

                (patteqns, pattsg) <- local (set tieExprEnvTypeTags ttypepatt) $ patternIToGraph patt
                (exprg, expreqn) <- local (set tieExprEnvTypeTags ttypeexpr) $ exprIToGraph expr

                equality .= symtable

                -- return ((pattsg, exprg), expreqn:patteqns) )
                return ((pattsg, exprg), expreqn ++ patteqns) )

                (NE.toList ttypetaggedcases)


        let expr' = ECase caseong (NE.fromList pattsgexprsg) $ fromJust $ Map.lookup ttypeinternal tagmap
            -- patt typing info
            ttypepatts      = NE.toList $ fmap (view exprTtype . fst . fst) ttypetaggedcases
            ttypeexprs      = NE.toList $ fmap (view exprTtype . fst . snd) ttypetaggedcases
            typeeqs = TypeEqnsExist 
                (ttypecaseon : ttypepatts ++ ttypeexprs)
                $ map (TypeEqnsEq . (TypeVar ttypecaseon [],) . flip TypeVar []) ttypepatts
                    -- ^ the type being cased on must be the same as the patterns 
                ++ map (TypeEqnsEq . (TypeVar ttype [],) . flip TypeVar []) ttypeexprs
                    -- ^ the type of the expression is the same as the result of the case
                ++ [TypeEqnsEq (TypeVar ttype [], TypeVar ttypeinternal [])]
                ++ concat eqns

        return (expr', [typeeqs])

    f (EVar ident ()) = do
        ttype <- gview exprTtype
        ttypeinternal <- gview exprTtypeInternal
        tagmap <- gview tieExprEnvTagTypeMap

        symtable <- guse equality
        lkup <- lift $ 
            ambiguousLookupCheck
            =<< filter (has (_2 % symEntryInfo % _SymLocalSeqVar)) 
            <$> querySymbolTableBnfcIdentName ident symtable 
        let ~(_, ~(SymEntry tag ~(SymLocalSeqVar ttypelkup))) = fromJust lkup
            eqns = TypeEqnsExist [] 
                [ TypeEqnsEq (TypeVar ttype [], TypeVar ttypelkup [])
                , TypeEqnsEq (TypeVar ttype [], TypeVar ttypeinternal []) ] 

        return 
            ( EVar (TaggedBnfcIdent ident tag) $ fromJust $ Map.lookup ttypelkup tagmap
            , bool [eqns] [] (isNothing lkup) )


patternsIAndExprsIToGraph :: 
    [([PatternI BnfcIdent], ExprI BnfcIdent)] ->
    TieFun ( [([PatternG TaggedBnfcIdent TypeTag], ExprG TaggedBnfcIdent TypeTag)] 
        , TypeEqns TaggedBnfcIdent TypeTag )
patternsIAndExprsIToGraph pattsandexps = do
    ttype <- gview exprTtype
    internalttype <- gview exprTtypeInternal
    tagmap <- gview tieExprEnvTagTypeMap

    ttypepattsandexps <- traverse (\n -> (,n) <$> lift freshExprTypeTags ) pattsandexps
    ~(pattsgexpsg, eqns) <- unzip <$> 
        traverse 
            (\(tag, pattsexp) -> 
                local (set tieExprEnvTypeTags tag) 
                $ patternIAndExprIToGraph pattsexp) 
            ttypepattsandexps


    let ttypephrases = map (view exprTtype . fst) ttypepattsandexps
        typeeqns = TypeEqnsExist ttypephrases $
            map (TypeEqnsEq . (TypeVar ttype [],) . flip TypeVar []) ttypephrases
            ++ eqns
    return (pattsgexpsg, typeeqns)

patternIAndExprIToGraph ::
    ([PatternI BnfcIdent], ExprI BnfcIdent) ->
    TieFun ( ([PatternG TaggedBnfcIdent TypeTag], ExprG TaggedBnfcIdent TypeTag)
        , TypeEqns TaggedBnfcIdent TypeTag )
patternIAndExprIToGraph (patts, expr) = do
    -- get the current symbol table since the symbol table
    -- needs to be reset after.
    -- symtab <- gview tieExprEnvSymbolTable
    symtab <- guse equality

    ttype <- gview exprTtype
    internalttype <- gview exprTtypeInternal
    tagmap <- gview tieExprEnvTagTypeMap

    -- tag the patterns expression with a type..
    ~(ttypespatts, (ttypesexpr, expr)) <- lift $ do
            ttypespatts <- traverse (const freshExprTypeTags) patts
            ttypesexpr <- freshExprTypeTags
            return (zip ttypespatts patts, (ttypesexpr, expr))


    -- equations, and graph pattern from the type patterns
    -- Note: this modifies the symbol table..
    -- (patteqns, pattsg) <- undefined
    ~(patteqns, pattsg) <- unwrappedPatternsIToGraph symtab ttypespatts

    (exprg, expreqns) <- local (set tieExprEnvTypeTags ttypesexpr) (exprIToGraph expr)

    equality .= symtab

    let ttypepatts = map (view exprTtype . fst ) ttypespatts
        ttypeexpr = ttypesexpr ^. exprTtype
        ttypeeqn = TypeEqnsExist (ttypeexpr : ttypepatts) $ 
                [ TypeEqnsEq 
                    ( _TypeVar # (ttype, [])
                    , TypeSeq $ TypeSeqArrF 
                        (map (flip TypeVar []) ttypepatts) 
                        (TypeVar ttypeexpr []) 
                    ) ]
                ++ patteqns ++ expreqns

    return ((pattsg, exprg), ttypeeqn)


exprsIToGraph :: 
    -- | Expression to convert to a graph
    [(ExprTypeTags, ExprI BnfcIdent)] ->
    -- | result
    TieFun [( ExprG TaggedBnfcIdent TypeTag, [TypeEqns TaggedBnfcIdent TypeTag] )]
exprsIToGraph ttypesexprs = do
    traverse 
        (\(tag, expr) -> local (set tieExprEnvTypeTags tag) $ exprIToGraph expr ) 
        ttypesexprs 

unwrappedExprsIToGraph ::
    -- | Expression to convert to a graph
    [(ExprTypeTags, ExprI BnfcIdent)] ->
    -- | result
    TieFun
        ( [ExprG TaggedBnfcIdent TypeTag]
        , [TypeEqns TaggedBnfcIdent TypeTag])
unwrappedExprsIToGraph ttypesexprs =  
    second mconcat . unzip <$> exprsIToGraph ttypesexprs

--------------------------------------------------
-- pattern compilation
--------------------------------------------------
patternsIToGraph :: 
    SymbolTable -> 
    [(ExprTypeTags, PatternI BnfcIdent)] -> 
    TieFun [ ( [TypeEqns TaggedBnfcIdent TypeTag] , PatternG TaggedBnfcIdent TypeTag) ]
patternsIToGraph symtable tagspatts = traverse 
    (\(tag, patt) -> 
        local (set tieExprEnvTypeTags tag) (patternIToGraph patt)) 
    tagspatts

unwrappedPatternsIToGraph ::
    SymbolTable -> 
    [(ExprTypeTags, PatternI BnfcIdent)] -> 
    TieFun ( [TypeEqns TaggedBnfcIdent TypeTag] , [PatternG TaggedBnfcIdent TypeTag] )
unwrappedPatternsIToGraph symtable patts = 
    (first mconcat <<< unzip) <$> patternsIToGraph symtable patts

patternIToGraph :: 
    PatternI BnfcIdent -> 
    TieFun ([TypeEqns TaggedBnfcIdent TypeTag] , PatternG TaggedBnfcIdent TypeTag)
patternIToGraph pattern = 
    f pattern
  where
    f :: PatternI BnfcIdent -> TieFun ([TypeEqns TaggedBnfcIdent TypeTag] , PatternG TaggedBnfcIdent TypeTag)
    f (PConstructor ident () ctsargs ()) = do
        symtable <- guse equality

        lkup <- lift $ 
            ambiguousLookupCheck
            =<< querySymbolTableSequentialPhrases
            <$> querySymbolTableBnfcIdentName ident symtable

        let (_, ~(SymEntry tag ~(SymPhrase phraseg))) = fromJust lkup

        -- check if it is a data type
        lift $ tell $ 
            bool [_ExpectedDataConstructor # ident] []
                (isNothing lkup || has _DataObj (phraseg ^. phraseGObjType))

        let expectedarity = length (phraseg ^. typePhraseFrom)
            actualarity = length ctsargs
        lift $ tell $ 
            bool [_ArityMismatch # (ident, expectedarity, actualarity)] []
                (isNothing lkup || expectedarity == actualarity)

        -- fresh type vars for the constructor args
        ttypesctsargs <- lift $ traverse (const freshExprTypeTags) ctsargs
        let ttypectsargs = map (view exprTtype) ttypesctsargs

        --  query the phrase substitutions from the graph
        ~(clausetype, ttypeargs, clausesubstitutions) <- lift . fmap fst . splitGraphGenCore $ 
            clauseSubstitutions (phraseg ^. typePhraseContext % phraseParent)
        -- let (clausetype, ttypeargs, clausesubstitutions) = evalState 
                -- (clauseSubstitutions (phraseg ^. typePhraseContext % phraseParent)) supply

        (ctsargstypeeqs, ctsargspatts) <- 
            unwrappedPatternsIToGraph symtable $ zip ttypesctsargs ctsargs

        ttype <- gview exprTtype
        internalttype <- gview exprTtypeInternal
        tagmap <- gview tieExprEnvTagTypeMap


        let typeeqs = TypeEqnsExist (ttypectsargs ++ ttypeargs) $
                [ TypeEqnsEq ( TypeVar ttype [] , clausetype )
                , TypeEqnsEq ( TypeVar ttype [], TypeVar internalttype []) ] 
                ++ zipWith g ttypectsargs (phraseg ^. typePhraseFrom)
                ++ ctsargstypeeqs
            g typetag ctsargtype = TypeEqnsEq
                ( _TypeVar # (typetag, [])
                , fromJust $ substitutesTypeGToTypeGTypeTag 
                    clausesubstitutions ctsargtype )
            pat' = PConstructor 
                (_TaggedBnfcIdent # (ident, tag)) 
                phraseg ctsargspatts (fromJust $ Map.lookup internalttype tagmap)

        return ( bool [typeeqs] [] (isNothing lkup), pat' )

        {-
    f ttype (PUnit ident ()) = do
        -- tag doesn't matter for constants...
        ident' <- tagBnfcIdent ident
        return 
            ( (mempty, [TypeEqnsEq (_TypeVar # (ttype, []), TypeSeq $ TypeUnitF ident')] )
            , PUnit ident' $ fromJust $ Map.lookup ttype tagmap )

    f ttype (PRecord recordphrases ()) = do
        symtable <- gview toGraphEnvSymbolTable
        (phrasestags, phrasesg@(focusedphraseg :| rstphraseg)) <- NE.unzip <$> 
            traverse (liftEither . flip lookupSeqPhrase symtable . fst) recordphrases

        -- the focused clause (should be the same of all the 
        -- phrases ideally....) we check this immediately after
        let focusedclauseg = focusedphraseg ^. typePhraseContext % phraseParent

        -- check if all destructors
        unless (all (CodataObj==) $ fmap (view phraseGObjType) phrasesg)
            undefined
            -- (throwError $ liftTieDefnsError (_ExpectedCodataDestructor # fmap fst recordphrases))

        -- check if all from the same codata clause
        unless (all (focusedclauseg ^. typeClauseName ==) $
            map (view $ typePhraseContext % phraseParent % typeClauseName) rstphraseg)
            undefined
            -- (throwError $ liftTieDefnsError (_ExpectedDestructorsFromSameClause # fmap fst recordphrases))

        -- check if the records (phrases) match the declaration in the 
        -- codata clause..
        unless (and $ 
                zipWith (\a b -> a ^. typePhraseName % taggedBnfcIdentName 
                                == b ^. _1 % bnfcIdentName)
                    (focusedclauseg ^. typeClausePhrases) 
                    (NE.toList recordphrases))
            undefined
            -- (throwError $ liftTieDefnsError (_IllegalRecordPhrases # recordphrases)) 

        -- get the required substitutions from a codata
        (clausetype, ttypeargs, clausesubstitutions) <- 
                clauseSubstitutions focusedclauseg
        -- fresh type variables for the phrases...
        ttypedtsargs <- traverse (const freshTypeTag) $ NE.toList phrasesg 

        ((dtsargsym, dtsargstypeeqs), dtsargspatts) <- unwrappedPatternsIToGraph tagmap 
            $ zip ttypedtsargs 
            $ map (snd . snd) 
            $ NE.toList recordphrases

        let syms = dtsargsym
            typeeqs = TypeEqnsExist (ttypedtsargs ++ ttypeargs) $
                [ _TypeEqnsEq # (_TypeVar # (ttype, []), clausetype ) ]
                ++ zipWith g ttypedtsargs (NE.toList phrasesg)
                ++ dtsargstypeeqs
            g ttypedts phraseg = _TypeEqnsEq # 
                ( _TypeVar # (ttypedts, [])
                , _TypeSeq # _TypeSeqArrF # 
                    (  fromJust $ traverse (substitutesTypeGToTypeGTypeTag 
                        clausesubstitutions) (phraseg ^. typePhraseFrom)
                    , fromJust $ substitutesTypeGToTypeGTypeTag 
                        clausesubstitutions (phraseg ^. typePhraseTo)
                    )
                )

            pat' = PRecord 
                ( NE.fromList $ getZipList $ 
                    (\ident tag phraseg destpat -> 
                        ( _TaggedBnfcIdent # (ident, tag)
                        , (phraseg, destpat)))
                    <$> ZipList (map fst $ NE.toList recordphrases)
                    <*> ZipList (NE.toList phrasestags)
                    <*> ZipList (NE.toList phrasesg)
                    <*> ZipList dtsargspatts
                )
                $ fromJust $ Map.lookup ttype tagmap

        return ( (syms, [typeeqs] ) , pat' )

    f ttype ( PTuple tuple@(tuple1, tuple2 :| tuples) () ) = do
        -- note: all these partial patterns are okay because
        -- we know for certain that these lists contain at least 2
        -- elements since we have (val, non empty list of values)...
        let tupleelems = tuple1:tuple2:tuples
        ttypetuples <- traverse (const freshTypeTag) tupleelems
        ((symtab, eqns), patts) <- unwrappedPatternsIToGraph tagmap 
            (zip ttypetuples tupleelems)
        let syms = symtab
            ttypetuple1:ttypetuplerest = ttypetuples
            typeeqs = TypeEqnsExist ttypetuples $
                [TypeEqnsEq 
                    ( TypeVar ttype []
                    , TypeSeq $ TypeTupleF 
                        ( TypeVar ttypetuple1 []
                        , NE.fromList $ map (flip TypeVar []) ttypetuplerest)
                    ) ]
                ++ eqns
            tuplepatt1:tuplepatts = patts
            pat' = PTuple (tuplepatt1, NE.fromList tuplepatts) 
                $ fromJust $ Map.lookup ttype tagmap
        return ( (syms, [typeeqs]), pat' )

-}
    f ( PVar ident () ) = do
        ttype <- gview (exprTypeTags % exprTtype)
        internalttype <- gview (exprTypeTags % exprTtypeInternal)

        tagmap <- gview tieExprEnvTagTypeMap
        ident' <- lift $ tagBnfcIdent ident
        let syms = [
                ( ident' ^. taggedBnfcIdentName
                , SymEntry (ident' ^. uniqueTag) $ SymLocalSeqVar ttype ) 
                ]
            typeeqs = [TypeEqnsEq (TypeVar ttype [], TypeVar internalttype [])]
            pat' = PVar ident' $ fromJust $ Map.lookup internalttype tagmap

        equality %= (
            ( ident' ^. taggedBnfcIdentName
            , SymEntry (ident' ^. uniqueTag) $ SymLocalSeqVar internalttype): )

        return ( typeeqs, pat' )

        {-
    f ttype (PNull ident ()) = do
        -- tags for null tags do not matter
        ident' <- tagBnfcIdent ident
        return ((mempty,[]), PNull ident' $ fromJust $ Map.lookup ttype tagmap)
        -}

    -- TODO
    f n = error $ "ERROR not implemented yet:" ++ show n