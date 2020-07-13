{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module MPLPasses.TieFunctionDef where

import Optics 
import Optics.State
import Optics.State.Operators

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST
import MPLPasses.SymbolTable
import MPLPasses.ToGraphTypes
import MPLPasses.ToGraphErrors

import MPLPasses.Unification

import Data.Functor.Foldable

import Data.Map ( Map (..) )
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Except

import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE
import Data.Maybe

import Control.Arrow

functionDefIToGraph ::
    FunctionDefnI BnfcIdent ->
    StateT ToGraphState 
        (Either ToGraphErrors) 
        (FunctionDefG TaggedBnfcIdent)
functionDefIToGraph (FunctionDefn funident funtype fundefn) = mdo
    funidenttag <- freshUniqueTag
    let funident' = _TaggedBnfcIdent # (funident, funidenttag)
    toGraphSymbolTable %= 
        ( (funident' ^. taggedBnfcIdentName
        , (_SymEntry # (funident' ^. uniqueTag,fungraph))):)
    symtable <- use toGraphSymbolTable
    fungraph <- undefined
    undefined

functionBodyToGraph :: 
    ( [PatternI BnfcIdent]
    , Expr [PatternI BnfcIdent] (Stmt (DefnI BnfcIdent))
        (TypeI BnfcIdent) () BnfcIdent ) ->
    StateT ToGraphState 
        (Either ToGraphErrors) 
        ( [PatternG TaggedBnfcIdent]
        , Expr [PatternG TaggedBnfcIdent] (Stmt (DefnG TaggedBnfcIdent))
            (TypeG TaggedBnfcIdent) (FunctionCallValueKnot TaggedBnfcIdent) TaggedBnfcIdent )
functionBodyToGraph = undefined


patternsIToGraph :: 
    [PatternI BnfcIdent] -> 
    StateT ToGraphState 
        (Either ToGraphErrors) 
        [PatternG TaggedBnfcIdent]
patternsIToGraph = undefined

patternIToGraph :: 
    Map TypeTag (TypeG TaggedBnfcIdent) -> 
    TypeTag -> 
    PatternI BnfcIdent -> 
    StateT ToGraphState 
        (Either ToGraphErrors) 
        ( (SymbolTable, [TypeEqns TaggedBnfcIdent TypeTag])
        , PatternG TaggedBnfcIdent)
patternIToGraph tagmap ttype pattern = 
    f ttype pattern
  where
    f ttype (PConstructor ident () ctsargs ()) = do
        (tag, phraseg) <- lookupSeqPhrase ident

        -- check if it is a data type
        unless 
            (has _DataObj (phraseg ^. phraseGObjType)) 
            (throwError $ liftFunctionError (_ExpectedDataConstructor # ident))
        -- check arity
        let expectedarity = length (phraseg ^. typePhraseFrom)
            actualarity = length ctsargs
        unless
            (expectedarity == actualarity)
            (throwError $ liftFunctionError 
                (_ArityMismatch # (ident, expectedarity, actualarity)))

        --  query the phrase substitutions from the graph
        (clausetype, ttypeargs, clausesubstitutions) <- 
                clauseSubstitutions (phraseg ^. typePhraseContext % phraseParent)

        -- fresh type vars for the constructor args
        ttypeCtsArgs <- traverse (const freshTypeTag) ctsargs

        ((ctsargssym, ctsargstypeeqs), ctsargspatts) <- 
                fs ttypeCtsArgs ctsargs

        let syms = []  
            typeeqs = TypeEqnsExist (ttypeCtsArgs ++ ttypeargs) $
                [ TypeEqnsEq ( _TypeVar # ttype
                    , clausetype ) ] 
                ++ zipWith g ttypeCtsArgs (phraseg ^. typePhraseFrom)
                ++ ctsargstypeeqs
            g typetag ctsargtype = TypeEqnsEq
                ( TypeVar typetag
                , fromJust $ substitutesTypeGToTaggedType 
                    clausesubstitutions ctsargtype )
            pat' = PConstructor 
                (_TaggedBnfcIdent # (ident, tag)) 
                phraseg ctsargspatts (fromJust 
                    $ Map.lookup ttype tagmap)
        return 
            ( (syms ++ ctsargssym, [typeeqs] )
            , pat')

    f ttype (PUnit ident ()) = do
        -- tag doesn't matter for constants...
        ident' <- tagBnfcIdent ident
        return 
            ( ([], [TypeEqnsEq (TypeVar ttype, TypeSeq $ TypeUnitF ident')] )
            , PUnit ident' $ fromJust $ Map.lookup ttype tagmap )

    f ttype (PRecord recordphrases () ()) = do
        (phrasestags, phrasesg@(focusedphraseg :| rstphraseg)) <- NE.unzip <$> 
            traverse (lookupSeqPhrase . fst) recordphrases

        -- the focused clause (should be the same of all the 
        -- phrases ideally....) we check this immediately after
        let focusedclauseg = focusedphraseg ^. typePhraseContext % phraseParent

        -- check if all destructors
        unless (all (CodataObj==) $ fmap (view phraseGObjType) phrasesg)
            (throwError $ liftFunctionError 
                (_ExpectedCodataDestructor # fmap fst recordphrases))

        -- check if all from the same codata clause
        unless (all (focusedclauseg ^. typeClauseName ==) $
            map (view $ typePhraseContext % phraseParent % typeClauseName) rstphraseg)
            (throwError $ liftFunctionError 
                (_ExpectedDestructorsFromSameClause # fmap fst recordphrases))

        -- check if the records (phrases) match the declaration in the 
        -- codata clause..
        unless (and $ 
                zipWith (\a b -> a ^. typePhraseName % taggedBnfcIdentName 
                                == b ^. _1 % bnfcIdentName)
                    (focusedclauseg ^. typeClausePhrases) 
                    (NE.toList recordphrases))
            (throwError $ liftFunctionError 
                (_IllegalRecordPhrases # recordphrases)) 

        (clausetype, ttypeargs, clausesubstitutions) <- 
                clauseSubstitutions focusedclauseg

        undefined


    -- traversal for multiple patterns 
    -- requires a pairing of the typetags and patterns (Which should be the same length -- check this yourself.
    -- returns (([symboltable], [typeeqs]), patterng)
    fs ttypetags patterns = first ( concat *** concat <<< unzip) . unzip <$> 
                traverse (uncurry f) (zip ttypetags patterns)

    {-
    | PRecord { _pRecordPhrase :: NonEmpty (ident , Pattern typedef calldef ident)
                , _pRecordCallDef :: calldef 
                , _pType :: typedef }
    | PList { _pList :: [Pattern typedef calldef ident], _pType :: typedef }
    | PTuple { _pTuple :: (Pattern typedef calldef ident, NonEmpty (Pattern typedef calldef ident)), _pType :: typedef }
    | PVar { _pVar :: ident, _pType :: typedef }
    | PString { _pString :: String, _pType :: typedef }
    | PInt { _pInt :: (ident, Int), _pType :: typedef }
    | PNull  { _pNull :: ident, _pType :: typedef }
    -}
        

lookupSeqPhrase :: 
    BnfcIdent -> 
    StateT ToGraphState 
        (Either ToGraphErrors) 
        (UniqueTag, TypePhraseG TaggedBnfcIdent)
lookupSeqPhrase ident = do
    symtable <- guse toGraphSymbolTable
    let candidates = filter ((ident ^. bnfcIdentName==) . fst) symtable
        res = helper candidates
    maybe (throwError errormsg) pure res
  where
    helper ((_, SymEntry tag (SymSeqPhrase n)):rst) = Just (tag, n)
    helper ((_, SymEntry _ _):rst) = helper rst
    helper [] = Nothing

    errormsg = liftFunctionError (_SeqPhraseNotInScope # ident)

clauseSubstitutions :: 
    ( MonadState s m 
    , HasUniqueTag s ) =>
    TypeClauseG TaggedBnfcIdent ->
    m ( TaggedType
      , [TypeTag]
      , [(TaggedBnfcIdent, Type () TaggedBnfcIdent TypeTag)] )
    -- ( Clause type of the of the statevar
    -- , fresh vars used to substitte
    -- , substition list of unique tags to corresponsing types
        -- ( this includes the state variables )
clauseSubstitutions clauseg = do
    -- get the sub args
    clauseArgSubs <- traverse 
        (\n -> second TypeTag . (n,) <$> freshUniqueTag) 
        (clauseg ^. typeClauseArgs) 

    let clausestatevartype = _TypeWithArgs # 
            ( clauseg ^.  typeClauseName
            , ()
            , map snd argsubstitions )
        clausegraphspine = NE.toList $ 
            clauseg ^.  typeClauseNeighbors % clauseGraph % clauseGraphSpine
        argsubstitions = map 
            (second TypeVar) 
            clauseArgSubs
        statevarsubstitiions = map 
            (\n -> 
                ( n ^. typeClauseStateVar
                , _TypeWithArgs # 
                    (n^. typeClauseStateVar, (), map snd argsubstitions ))) 
            clausegraphspine

    return ( clausestatevartype
            , map snd clauseArgSubs
            , statevarsubstitiions ++ argsubstitions)


