{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
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
import Data.Foldable
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
import Control.Monad.Reader
import Control.Applicative

import Debug.Trace 

data TieDefnsTState = TieDefnsTState {
    _tieDefnsStateSymbolTable :: SymbolTable
    , _tieDefnsStateSeqTypeEqnsPkg :: TieDefnsTypeEqnsPkg
}

data TieDefnsTypeEqnsPkg = TieDefnsTypeEqnsPkg  {
    _tieDefnsTypeForall :: [TypeTag]
    , _tieDefnsTypeExist :: [TypeTag]
    , _tieDefnsTypeEqns :: [TypeEqns TaggedBnfcIdent TypeTag]
}  

instance Semigroup TieDefnsTypeEqnsPkg where
    TieDefnsTypeEqnsPkg a0 b0 c0 <> TieDefnsTypeEqnsPkg a1 b1 c1 =
        TieDefnsTypeEqnsPkg (a0 <> a1) (b0 <> b1) (c0 <> c1)

instance Monoid TieDefnsTypeEqnsPkg where
    mempty = TieDefnsTypeEqnsPkg [] [] []
    


$(makeLenses ''TieDefnsTState)
$(makeLenses ''TieDefnsTypeEqnsPkg)


newtype TieDefnsT m a = 
    TieDefnsT { unTieDefnsT :: RWST 
            TagTypeMap 
            [DefnG TaggedBnfcIdent TypeTag] 
            TieDefnsTState 
            m a }
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadState TieDefnsTState
    , MonadReader TagTypeMap 
    , MonadWriter [DefnG TaggedBnfcIdent TypeTag] 
    , MonadRWS TagTypeMap [DefnG TaggedBnfcIdent TypeTag] TieDefnsTState  
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

type TieFun a = TieFunT GraphGenCore a

runTieDefnsT :: 
    TagTypeMap ->
    TieDefnsTState ->
    TieDefns a -> 
    GraphGenCore (a, TieDefnsTState, [DefnG TaggedBnfcIdent TypeTag])
runTieDefnsT tagmap st (TieDefnsT m) = runRWST m tagmap st

runTieFunT ::
    TieExprEnv ->
    SymbolTable ->
    TieFun a ->
    GraphGenCore (a, SymbolTable)
runTieFunT env sym = flip runStateT sym 
    . flip runReaderT env 
    . unTieFunT

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
    rec ((), st, defs') <- lift $ 
            runTieDefnsT 
            tagmap 
            (TieDefnsTState (wsyms++symtab) mempty)
            (defnsToGraph (NE.toList defs)) 
        let seqeqnspkg = st ^. tieDefnsStateSeqTypeEqnsPkg
            seqeqns = TypeEqnsForall (seqeqnspkg ^. tieDefnsTypeForall)
                [TypeEqnsExist (seqeqnspkg ^. tieDefnsTypeExist) (seqeqnspkg ^. tieDefnsTypeEqns)]
            seqpkg = solveTypeEq (trace (pprint seqeqns) seqeqns)
            -- seqpkg = solveTypeEq seqeqns
            Right seqpkg' = seqpkg

            -- concpkg = solveTypeEq (uncurry TypeEqnsExist $ first (map (view exprTtypeInternal)) concdmyeqns)
            -- Right concpkg' = concpkg

            tagmap = packageToTagMap $ seqpkg' -- <> concpkg'

    tell $ either pure (const []) seqpkg
    -- tell $ either pure (const []) concpkg

    return $ Stmt (NE.fromList defs') wstmts'

defnsToGraph :: 
    [DefnI BnfcIdent] ->
    TieDefns ()
        -- Sequential and concurrent..
defnsToGraph [] = return mempty
defnsToGraph (a:as) = case a ^. unDefnI of 
    DataDefn ival -> mdo
        objectDefIToGraph 
            ival
            [ lift $ typeClauseArgsSanityCheck ival , lift $ phraseToVarsAreStateVarCheck ival ]
            (filter sequentialClausesPredicate symtable)
            DataObj

        defnsToGraph as
        symtable <- guse tieDefnsStateSymbolTable
        return ()

    CodataDefn ival -> mdo
        objectDefIToGraph 
            ival
            [ lift $ typeClauseArgsSanityCheck ival, lift $ codataStateVarOccurenceCheck ival ]
            (filter sequentialClausesPredicate symtable)
            CodataObj

        defnsToGraph as
        symtable <- guse tieDefnsStateSymbolTable
        return ()

    ProtocolDefn n -> mdo
        undefined
        -- lift $ typeClauseArgsSanityCheck n 
        -- lift $ phraseToVarsAreStateVarCheck n

    CoprotocolDefn n -> mdo
        undefined
        -- lift $ typeClauseArgsSanityCheck n 
        -- lift $ phraseFromVarsAreStateVarCheck n 
 
    FunctionDecDefn (FunctionDefn funident funtype funbody) -> mdo  
        funident' <- lift $ tagBnfcIdent funident
        let funbody' = NE.toList funbody
        ttypes <- lift freshExprTypeTags

        {-
        funtype' <- lift $ case funtype of
            Just funtype -> Just 
                <$> evalStateT (annotateTypeIToTypeGAndScopeFreeVars 
                $ (TypeSeq . uncurry TypeSeqArrF) funtype) symtab 
            Nothing -> return Nothing
            -}
        funtype' <- lift $ traverse 
                ( flip evalStateT 
                    (filter sequentialClausesPredicate symtab)
                . ( annotateTypeIToTypeGAndScopeFreeVars 
                   . (TypeSeq . uncurry TypeSeqArrF))
                    ) funtype
        -- splitting is not needed here...
        (forallvars, forallsubs) <- lift . join . fmap writer . splitGraphGenCore $ do
                mfuntypefreevarsandsubs <- traverse genTypeGSubs funtype'
                return $ fromMaybe ([],[]) mfuntypefreevarsandsubs 

        let ttype = ttypes ^. exprTtype
            ttypeinternal = ttypes ^. exprTtypeInternal

        tagmap <- gview equality 

        tieDefnsStateSymbolTable %= (
            ( funident' ^. taggedBnfcIdentName
            , SymEntry (funident' ^. uniqueTag) (funident' ^. taggedBnfcIdentPos)
            $ SymCall 
                -- if we have an explicit type, just use that, otherwise
                -- give it the dummy type... (better error messages lower down where
                -- the issue really is..
                (maybe (SymCallDummyTypeVar ttype) SymCallExplicitType funtype')
                (FunctionKnot fun')): )

        ttypespattsandexps <- lift $ traverse (const freshExprTypeTags) funbody'

        ~((pattsgexpsg, eqns), _) <- lift 
            $ runTieFunT (TieExprEnv ttypes tagmap) symtab 
            $ fmap unzip <$> 
                traverse 
                    (\(tag, pattsexp) -> 
                        local (set tieExprEnvTypeTags tag) 
                        $ patternsIAndExprIToGraph pattsexp) 
                    $ zip ttypespattsandexps funbody'

        let fun' = FunctionDefn 
                funident' 
                (fromJust $ Map.lookup ttypeinternal tagmap) 
                (NE.fromList pattsgexpsg)

            ttypephrases = map (view exprTtype) ttypespattsandexps
            {-
            typeeqns = TypeEqnsExist ttypephrases $
                map (TypeEqnsEq . (TypeVar ttype [],) . flip TypeVar []) ttypephrases ++ eqns
                -}
            typeeqns = 
                TypeEqnsEq (TypeVar ttype [], TypeVar ttypeinternal []):
                -- if there is for all quantification, we need to include the equality to the
                -- user provided type
                toList (fmap (TypeEqnsEq . (TypeVar ttype [],))
                    $ join 
                    $ substitutesTypeGToTypeGTypeTag forallsubs 
                    <$> funtype') 
                -- Ensure that each of the type phrases is equal to the parent type.
                ++ map (TypeEqnsEq . (TypeVar ttype [],) . flip TypeVar []) ttypephrases 
                -- collected equations context 
                ++ eqns


        tieDefnsStateSeqTypeEqnsPkg % tieDefnsTypeForall %= ((forallvars)++)
        tieDefnsStateSeqTypeEqnsPkg % tieDefnsTypeExist %= ((ttype:ttypephrases)++)
        tieDefnsStateSeqTypeEqnsPkg % tieDefnsTypeEqns %= (typeeqns++)

        tell [FunctionDecDefG fun']


        defnsToGraph as

        symtab <- guse tieDefnsStateSymbolTable

        return ()

    ProcessDecDefn n -> undefined

-- function defn to graph....

objectDefIToGraph ival checks symtable objtype = do
    sequenceA checks
    ~clauseg <- lift $ tieTypeClauseGraph symtable objtype ival
    let defn' = ObjectG clauseg :: DefnG TaggedBnfcIdent TypeTag

    tieDefnsStateSymbolTable %= ((collectSymEntries defn')++)

    tell [defn']

    return () 


patternsIAndExprsIToGraph :: 
    [([PatternI BnfcIdent], ExprI BnfcIdent)] ->
    TieFun ( [([PatternG TaggedBnfcIdent TypeTag], ExprG TaggedBnfcIdent TypeTag)] 
        , TypeEqns TaggedBnfcIdent TypeTag )
patternsIAndExprsIToGraph pattsandexps = do
    ttype <- gview exprTtype
    internalttype <- gview exprTtypeInternal
    tagmap <- gview tieExprEnvTagTypeMap

    ttypespattsandexps <- traverse (\n -> (,n) <$> lift freshExprTypeTags ) pattsandexps
    ~(pattsgexpsg, eqns) <- unzip <$> 
        traverse 
            (\(tag, pattsexp) -> 
                local (set tieExprEnvTypeTags tag) 
                $ patternsIAndExprIToGraph pattsexp) 
            ttypespattsandexps


    let ttypephrases = map (view exprTtype . fst) ttypespattsandexps
        typeeqns = TypeEqnsExist ttypephrases $
            map (TypeEqnsEq . (TypeVar ttype [],) . flip TypeVar []) ttypephrases
            ++ eqns
    return (pattsgexpsg, typeeqns)

patternsIAndExprIToGraph ::
    ([PatternI BnfcIdent], ExprI BnfcIdent) ->
    TieFun ( ([PatternG TaggedBnfcIdent TypeTag], ExprG TaggedBnfcIdent TypeTag)
        , TypeEqns TaggedBnfcIdent TypeTag )
patternsIAndExprIToGraph (patts, expr) = do
    -- get the current symbol table since the symbol table
    -- needs to be reset after.
    -- symtab <- gview tieExprEnvSymbolTable
    symtab <- guse equality

    ttype <- gview exprTtype
    ttypeinternal <- gview exprTtypeInternal
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
                    ( TypeVar ttype []
                    , TypeSeq $ TypeSeqArrF 
                        (map (flip TypeVar []) ttypepatts) 
                        (TypeVar ttypeexpr []) 
                    ) 
                , TypeEqnsEq 
                    ( TypeVar ttype [] , TypeVar ttypeinternal []) 
                    ]
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

------------------------
-- compiling expressions...
------------------------


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
        symtable <- guse equality

        lkup <- lift $ 
            runSymbolTableQuery 
            (queryBnfcIdentName ident >> querySequentialPhrases >> queryChecks >> symbolTableQuery) 
            symtable

        let ~(Just (SymEntry tag pos ~(SymPhrase phraseg))) = lkup

        ttype <- gview exprTtype 
        internalttype <- gview exprTtypeInternal 

        tagmap <- gview tieExprEnvTagTypeMap 

        let expectedarity = length (phraseg ^. typePhraseFrom)
            actualarity   = length args
        lift $ tell $ bool 
                [_ArityMismatch # (ident, expectedarity, actualarity)]
                []
                (isNothing lkup || expectedarity == actualarity)

        --  query the phrase substitutions from the graph
        ~(clausetype, ttypeargs, clausesubstitutions) <- lift . fmap fst . splitGraphGenCore $ 
                clauseSubstitutions (phraseg ^. typePhraseContext % phraseParent)
        
        -- fresh type vars for the constructor args
        ttypesctsargs <- lift $ traverse (const freshExprTypeTags) args

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
        return ( expr', bool [typeeqs] [] $ isNothing lkup )

    f (ECase caseon cases ()) = do
        ttype <- gview exprTtype
        ttypeinternal <- gview exprTtypeInternal
        tagmap <- gview tieExprEnvTagTypeMap

        -- converting the case on to a graph...
        -- ttypecaseon <- lift freshTypeTag
        ttypescaseon <- lift freshExprTypeTags 
        (caseong, caseongeqns) <- local (set tieExprEnvTypeTags ttypescaseon) $ exprIToGraph caseon

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
            ttypepatts  = NE.toList $ fmap (view exprTtype . fst . fst) ttypetaggedcases
            ttypeexprs  = NE.toList $ fmap (view exprTtype . fst . snd) ttypetaggedcases
            ttypecaseon = ttypescaseon ^. exprTtype
            typeeqs = TypeEqnsExist 
                (ttypecaseon : ttypepatts ++ ttypeexprs)
                $ [ TypeEqnsEq (TypeVar ttype [], TypeVar ttypeinternal []) ]
                ++ map (TypeEqnsEq . (TypeVar ttypecaseon [],) . flip TypeVar []) ttypepatts
                    -- ^ the type being cased on must be the same as the patterns 
                ++ map (TypeEqnsEq . (TypeVar ttype [],) . flip TypeVar []) ttypeexprs
                    -- ^ the type of the expression is the same as the result of the case
                ++ caseongeqns
                ++ concat eqns

        return (expr', [typeeqs])

    f (EVar ident ()) = do
        ttype <- gview exprTtype
        ttypeinternal <- gview exprTtypeInternal
        tagmap <- gview tieExprEnvTagTypeMap

        symtable <- guse equality

        lkup <- lift $ 
            runSymbolTableQuery 
            (queryBnfcIdentName ident >> queryLocalSeqVar >> queryChecks >> symbolTableQuery) 
            symtable


        let ~(SymEntry tag pos ~(SymLocalSeqVar ttypelkup)) = fromJust lkup
            eqns = 
                [ TypeEqnsEq (TypeVar ttype [], TypeVar ttypelkup [])
                , TypeEqnsStableRef (ttypeinternal, TypeVar ttype []) ] 

        return 
            ( EVar (TaggedBnfcIdent ident tag) $ fromJust $ Map.lookup ttypeinternal tagmap
            , bool eqns [] (isNothing lkup) )

    f (ECall ident () args ()) = do
        ttype <- gview exprTtype
        ttypeinternal <- gview exprTtypeInternal
        tagmap <- gview tieExprEnvTagTypeMap

        symtable <- guse equality

        lkup <- lift $ 
            runSymbolTableQuery 
            (queryBnfcIdentName ident >> querySeqCallFuns >> queryChecks >> symbolTableQuery) 
            symtable


        let ~(Just ~(SymEntry tag pos (SymCall calltype calldef))) = lkup

        ~(calltype', callttypeargs) <- lift . fmap fst . splitGraphGenCore $ case calltype of
            SymCallDummyTypeVar t -> pure $ (TypeVar t [], [])
            SymCallExplicitType t -> do
                (args, argssubs ) <- genTypeGSubs t
                return (fromJust $ substitutesTypeGToTypeGTypeTag argssubs t, args)

        -- compile the arguments..
        ttypesargs <- lift $ traverse (const freshExprTypeTags) args
        (argsexprgs, argseqns) <- unwrappedExprsIToGraph $ zip ttypesargs args

        -- (calltypevars, calltypesubs) <- lift . fmap fst . splitGraphGenCore $ genTypeGSubs (traceShowId calltype)

        -- todo 
        let ttypeargs = map (view exprTtype) ttypesargs
            eqns = TypeEqnsExist ttypeargs $
                [ TypeEqnsEq (TypeVar ttype [], TypeVar ttypeinternal [])
                , TypeEqnsEq 
                    -- ( fromJust (substitutesTypeGToTypeGTypeTag calltypesubs calltype)
                    ( calltype'
                    , TypeSeq $ TypeSeqArrF (map (flip TypeVar []) ttypeargs) (TypeVar ttype []) )
                    ]
                ++ argseqns
                {-
                ++ zipWith (\ttype -> TypeEqnsEq . (TypeVar ttype [],)) 
                    ttypeargs 
                    (map (fromJust . substitutesTypeGToTypeGTypeTag calltypesubs) froms)
                ++ argseqns
                -}

        return 
            ( ECall 
                (TaggedBnfcIdent ident tag) 
                calldef argsexprgs 
                $ fromJust $ Map.lookup ttypeinternal tagmap
            , bool [eqns] [] (isNothing lkup) )

    f n = error $ show n
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
            runSymbolTableQuery 
            (queryBnfcIdentName ident >> querySequentialPhrases >> queryChecks >> symbolTableQuery) 
            symtable

        let ~(SymEntry tag pos ~(SymPhrase phraseg)) = fromJust lkup

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

        -}

    f (PRecord recordphrases ()) = do
        undefined
        {-
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
        -}

    {-
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
        let typeeqs = [TypeEqnsEq (TypeVar ttype [], TypeVar internalttype [])]
            pat' = PVar ident' $ fromJust $ Map.lookup internalttype tagmap

        equality %= (
            ( ident' ^. taggedBnfcIdentName
            , SymEntry (ident' ^. uniqueTag) (ident' ^. taggedBnfcIdentPos) 
                $ SymLocalSeqVar ttype): )

        return ( typeeqs, pat' )

        {-
    f ttype (PNull ident ()) = do
        -- tags for null tags do not matter
        ident' <- tagBnfcIdent ident
        return ((mempty,[]), PNull ident' $ fromJust $ Map.lookup ttype tagmap)
        -}

    -- TODO

