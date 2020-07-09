{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MPLPasses.ToGraph where

import Optics 

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST
import MPLPasses.ToGraphErrors 
import MPLPasses.SymbolTable

import MPLPasses.ToGraphTypes
import Control.Monad.RWS

import MPLUtil.Data.Tuple.Optics
import MPLUtil.Data.Either.AccumEither

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Control.Monad.State
import Control.Monad.Except

import Debug.Trace

data SymEntry =
    SymTypeClause (TypeClauseKnot SymEntry String)
    | SymTypeVar
  deriving Show

type Graph a = RWST
    [TypeClauseKnot SymEntry String] 
    [TypeClauseKnot SymEntry String] 
    [(String, SymEntry)] 
    (Either String)
    a

testingtie :: 
    [TypeClause () () () String] ->
    Graph ()
    -- State [(String, SymEntry)] [TypeClauseKnot SymEntry String]  
testingtie [] = return ()
testingtie ((TypeClause a args stv phrases ()):as) = mdo
    env <- ask
    let clause = TypeClause a args stv phrases' (ClausesKnot env)
    tell [ clause ]
    modify ((stv, symtab):)
    testingtie as
    let symtab = SymTypeClause clause
    phrases' <- mapM (f clause) phrases
    return ()

  where
    f clause (TypePhrase () ident [] toty) = mdo
        toty' <- getTypeVar toty
        return $ TypePhrase 
            (ClausePhraseKnot clause)
            ident
            []
            toty'

    getTypeVar :: 
        Type () String -> 
        Graph (Type SymEntry String)
    getTypeVar (TypeWithArgs ident () args) = mdo
        ~v <- gets (lookup ident )
        -- this is very important
        case v of 
            Just v' -> return (TypeWithArgs ident v' [])
            Nothing -> throwError $ "AHAHAH IN ETHERERROR"

handtiedd = [clause]
  where
    clause = TypeClause "Clause" [] "ST"
        [ TypePhrase 
            (ClausePhraseKnot $ clause)
            "Phrase"
            []
            (TypeWithArgs "ST" (SymTypeClause $ clause) [])
        ]
        (ClausesKnot handtiedd)


heknewthewholetime = [
    TypeClause "Clause" [] "ST"
        [ TypePhrase ()
            "Phrase"
            []
            (TypeWithArgs "Potato" () [])
        ]
        ()
    , TypeClause "Clause2" [] "Potato"
        [ TypePhrase ()
            "Phrase2"
            []
            (TypeWithArgs "ST" () [])
        ]
        ()
    ]

runRws rws = w
  where
    ((), s, w) = case runRWST rws w [] of
                    Right a -> a
                    Left str -> error str

evaluated = runRws (testingtie  heknewthewholetime )

isitreal = case evaluated of
            [TypeClause _ _ _ phrase b, _ ] -> 
                case phrase of
                    [TypePhrase ctxt ident [] 
                        (TypeWithArgs argident to [])] ->  
                            case to of
                                SymTypeClause n -> n ^. typeClauseName
isitreal2 = case evaluated of
            [TypeClause _ _ _ phrase b, _ ] -> 
                case phrase of
                    [TypePhrase ctxt ident [] 
                        (TypeWithArgs argident to [])] ->  
                            case to of
                                SymTypeClause n -> ctxt
    


{-
progInterfaceToGraph :: 
    Prog (DefnI BnfcIdent) ->
    Prog (DefnG TaggedBnfcIdent) 
progInterfaceToGraph = undefined

runTieKnots :: 
    ( AsToGraphErrors e 
    , HasToGraphEnv env
    , HasToGraphState s ) =>
    env -> s ->
    RWS env (Prog (Either (NonEmpty e) (DefnG TaggedBnfcIdent))) s () ->
    (Prog (Either (NonEmpty e) (DefnG TaggedBnfcIdent)))
runTieKnots env s mn = w
  where
    ((), s, w) = runRWS mn env s

type GraphKnotTie e r s a = RWS r (AccumEither (NonEmpty e) (Prog ( (DefnG TaggedBnfcIdent)))) s a

{-
-- need to add an overlapping decs check..
makeGraph :: 
    ( AsToGraphErrors e 
    , HasToGraphEnv env
    , HasToGraphState s ) =>
    Prog (DefnI BnfcIdent) -> 
    GraphKnotTie e env s (AccumEither (NonEmpty e) [(Stmt (DefnG TaggedBnfcIdent))])
makeGraph n = case n ^? prog % _Cons of
    Just (a, as) -> do
        (scopes, stmt) <- makeGraphStmt a 
        tell (review _Prog . pure <$> stmt)
        addToTopScope scopes
        makeGraph (review _Prog as)
    Nothing -> return mempty

makeGraphStmt :: 
    ( AsToGraphErrors e 
    , HasToGraphEnv env
    , HasToGraphState s ) =>
    Stmt (DefnI BnfcIdent) -> 
    GraphKnotTie e env s  (Scope, AccumEither (NonEmpty e) (Stmt (DefnG TaggedBnfcIdent)))
makeGraphStmt stmt = withNewScope $ mdo
    wheres <- makeGraph (review _Prog (stmt ^. stmtWhereBindings))
    undefined

makeDataGraph ::
    forall e env s .
    ( AsToGraphErrors e 
    , HasToGraphEnv env
    , HasToGraphState s ) =>
    TypeClausesPhrases () () TypePhraseFromTo BnfcIdent ->
    GraphKnotTie e env s (SeqObjectG TaggedBnfcIdent) 
makeDataGraph defn = res
  where
    res = traverse f defn

    f :: TypeClausePhrase () () TypePhraseFromTo BnfcIdent -> 
            GraphKnotTie e env s
            (TypeClauseKnot (TypePhraseFromTo (SeqClauseG TaggedBnfcIdent) TaggedBnfcIdent) TaggedBnfcIdent)
    f typeclause = mdo
        ((nametagged, statevartagged, phrasenames, args), typeclause') <- collectAndTag typeclause 

        typeclause'' <- withNewScope $ do
            addToTopScope $ 
                [ ( statevartagged ^. taggedBnfcIdentName 
                , (statevartagged ^. taggedBnfcIdentTag, SymSeqClause typeclause'')) ] 
                ++ (map (\n -> (n ^. taggedBnfcIdentName, (n ^. taggedBnfcIdentTag, SymTypeArgVar))) args)
            return (tieSeqObjectKnot res typeclause')

        -- addToTopScope (map (over _1 (view (stringPos % _1)) . view (re associated) . (,SymTypeArgVar)) args)

        -- addToTopScope [(statevar ^. stringPos % _1, (statevartag, typeclause''))]

        -- let typeclause' = typeclause & partsOf traversed .~ (map (review _TaggedBnfcIdent) ([nametagged] ++ args ++ [(statevar, statevartag)]))
        undefined

    collectAndTag :: TypeClausePhrase () () TypePhraseFromTo BnfcIdent -> 
        GraphKnotTie e env s ( (TaggedBnfcIdent, TaggedBnfcIdent, [TaggedBnfcIdent], [TaggedBnfcIdent]),
            TypeClausePhrase () () TypePhraseFromTo TaggedBnfcIdent)
    collectAndTag typeclause = do
        nametagged <- review _TaggedBnfcIdent <$> ((typeclause ^. typeClauseName,) <$> freshUniqueTag)
        statevartagged <- review _TaggedBnfcIdent <$> ((typeclause ^. typeClauseStateVar,) <$> freshUniqueTag)
        args <- traverse (\n -> review _TaggedBnfcIdent <$> ((n,) <$> freshUniqueTag)) (typeclause ^. typeClauseArgs) 
        phrasenames <- traverse (\n -> review _TaggedBnfcIdent <$> ((n ^. typePhraseName,) <$> freshUniqueTag)) 
                (typeclause ^. typeClausePhrases )  
        return ((nametagged, statevartagged, phrasenames, args), 
                        typeclause {
                            _typeClauseName = nametagged
                            , _typeClauseArgs = args
                            , _typeClauseStateVar = statevartagged
                            , _typeClausePhrases = zipWith (\a b -> b & typePhraseName .~ a )
                                phrasenames (typeclause ^. typeClausePhrases)
                            , _typeClauseNeighbors = ()
                        }
                    )

class TieTypeClausePhrase phrase where
    tieSeqObjectKnot :: phrase
    -}
{-
    tieSeqObjectKnot :: 
        ( AsToGraphErrors e 
        , HasToGraphEnv env
        , HasToGraphState s 
        , TieTypeClausePhrase phrase ) =>
        SeqObjectG TaggedBnfcIdent  -> 
        TypeClausePhrase () () phrase TaggedBnfcIdent -> 
        m (SeqObjectG TaggedBnfcIdent)-}

    
    -- defnname <- freshUniqueTag
    -- addToTopScope [(defn ^. typeClauseName, (defnname, _ )) ]
    -- return (defn ^. typeClauseName)
    -- undefined

-}
