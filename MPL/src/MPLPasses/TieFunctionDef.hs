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
    PatternI BnfcIdent -> 
    StateT ToGraphState 
        (Either ToGraphErrors) 
        ( (SymbolTable, [TypeEqns TaggedBnfcIdent TypeTag], TypeTag)
        , PatternG TaggedBnfcIdent)
patternIToGraph tagmap pattern = do
    typetag <- freshTypeTag
    fix f typetag pattern
  where
    f :: (TypeTag -> PatternI BnfcIdent -> StateT ToGraphState (Either ToGraphErrors) ((SymbolTable, [TypeEqns TaggedBnfcIdent TypeTag], TypeTag), PatternG TaggedBnfcIdent)) -> 
        TypeTag -> 
        PatternI BnfcIdent -> 
        StateT ToGraphState (Either ToGraphErrors)
                  ((SymbolTable, [TypeEqns TaggedBnfcIdent TypeTag], TypeTag),
                   PatternG TaggedBnfcIdent)
    f fx typetag (PConstructor ident () args ()) = undefined
    {-
    f :: PatternF () () 
        BnfcIdent 
        (StateT ToGraphState (Either ToGraphErrors) ((SymbolTable, [TypeEqns TaggedBnfcIdent TypeTag], TypeTag), PatternG TaggedBnfcIdent)) -> 
        StateT 
            ToGraphState
            (Either ToGraphErrors)
            ( (SymbolTable, [TypeEqns TaggedBnfcIdent TypeTag], TypeTag)
            , PatternG TaggedBnfcIdent)
    f (PConstructorF ident () args ()) = do
        ((symtab, eqns, typetags), patts) <- fmap 
                        (first (over _2 concat . over _1 concat . unzip3) 
                        . unzip)
                        (sequenceA args)
        (tag, phraseg) <- lookupSeqPhrase ident

        -- Some error checking TODO -- change so that this uses AccumEither
        -- for better error messages...

        -- check that it must be a data object
        when 
            (has _DataObj (phraseg ^. phraseGObjType)) 
            (throwError $ liftFunctionError (_ExpectedDataConstructor # ident))
        -- check arity
        let expectedarity = length (phraseg ^. typePhraseFrom)
            actualarity = length args
        when 
            (expectedarity /=  actualarity)
            (throwError $ liftFunctionError 
                (_ArityMismatch # (ident, expectedarity, actualarity)))

        (phraseclausetype, freshargsmapping, substitions) <- 
                phraseSubstitutions phraseg

        -- type tags for the type equations
        typetag' <- freshTypeTag
        typetags' <- mapM (const (freshTypeTag)) typetags

        -- type equations
        let neweqns = zipWith 
                (\a b -> TypeEqnsEq 
                    ( TypeVar a
                    , fromJust $ forceSubstitutes substitions b 
                    ) )
                typetags'
                (phraseg ^. typePhraseFrom) 

        -- the new constructor with the type...
        let cts' = _PConstructor #
                    ( _TaggedBnfcIdent # (ident, tag)
                    , phraseg
                    , patts
                    , fromJust $ Map.lookup typetag' tagmap )

        return ((symtab, neweqns ++ eqns, typetag'), cts')

        -}
    {-
    f (PUnitF ident ()) = do
        -- TODO constants unique tag do not matter
        typetag' <- freshTypeTag
        ident' <- tagBnfcIdent ident
        return ( ([]
                , [TypeEqnsEq (TypeVar typetag', TypeSeq $ TypeUnitF ident')]
                , typetag' )
                , PUnit ident' $ fromJust $ Map.lookup typetag' tagmap)

    f (PRecordF phrases () ()) = do
        undefined
        -}
    {-
    | PRecord { _pRecordPhrase :: NonEmpty (ident , Pattern typedef calldef ident)
                , _pRecordCallDef :: calldef 
                , _pType :: typedef }
    | PList { _pList :: [Pattern typedef calldef ident], _pType :: typedef }
    | PTuple { 
            _pTuple :: (Pattern typedef calldef ident, NonEmpty (Pattern typedef calldef ident))
            , _pType :: typedef }
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

phraseSubstitutions :: 
    ( MonadState s m 
    , HasUniqueTag s ) =>
    TypePhraseG TaggedBnfcIdent ->
    m ( TaggedType
      , [TypeTag]
      , [(TaggedBnfcIdent, Type () TaggedBnfcIdent TypeTag)] )
    -- ( Clause type of the phrase of the statevar
    -- , fresh vars used to substitte
    -- , substition list of unique tags to corresponsing types
        -- ( this includes the state variables )
phraseSubstitutions phraseg = do
    -- get the sub args
    clauseArgSubs <- traverse 
        (\n -> second TypeTag . (n,) <$> freshUniqueTag) 
        (phraseg ^. typePhraseContext % phraseParent % typeClauseArgs) 

    let clausestatevartype = _TypeWithArgs # 
            ( phraseg ^.  typePhraseContext % phraseParent % typeClauseName
            , ()
            , map snd argsubstitions )
        clausegraphspine = NE.toList $ phraseg ^. typePhraseContext 
            % phraseParent 
            % typeClauseNeighbors 
            % clauseGraph 
            % clauseGraphSpine
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


