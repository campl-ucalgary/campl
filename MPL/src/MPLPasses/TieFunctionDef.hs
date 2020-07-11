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
        ((SymbolTable, [TypeEqns TypeTag], TypeTag), PatternG TaggedBnfcIdent)
patternIToGraph tagmap = cata f 
  where
    f :: PatternF () () BnfcIdent (StateT ToGraphState (Either ToGraphErrors) ((SymbolTable, [TypeEqns TypeTag], TypeTag), PatternG TaggedBnfcIdent)) -> StateT
                  ToGraphState
                  (Either ToGraphErrors)
                  ((SymbolTable, [TypeEqns TypeTag], TypeTag),
                   PatternG TaggedBnfcIdent)

    f (PConstructorF ident () args ()) = do
        ((symtab, eqns, tags), patts) <- fmap 
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

        typeArgSubs <- traverse 
            (\n -> second TypeVar . (n,) <$> freshUniqueTag) 
            (phraseg ^. phraseGClauseTypeArgs) 


        let cts' = _PConstructor #
                    ( _TaggedBnfcIdent # (ident, tag)
                    , phraseg
                    , patts
                    , fromJust $ Map.lookup (TypeTag tag) tagmap )
        undefined
        -- return cts'

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

