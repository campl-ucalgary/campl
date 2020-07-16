{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module MPLPasses.TieTermUtils where

import Optics 
import Optics.State
import Optics.State.Operators

import Control.Applicative

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST
import MPLPasses.SymbolTable
import MPLPasses.ToGraphTypes
import MPLPasses.ToGraphErrors
import MPLPasses.Unification

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
import Debug.Trace

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

    errormsg = liftToGraphErrors (_SeqPhraseNotInScope # ident)

clauseSubstitutions :: 
    ( MonadState s m 
    , HasUniqueTag s ) =>
    TypeClauseG TaggedBnfcIdent ->
    m ( TypeGTypeTag
      , [TypeTag]
      , [(TaggedBnfcIdent, TypeGTypeTag)] )
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
            , TypeClauseNode clauseg
            , map snd argsubstitions )
        clausegraphspine = NE.toList $ 
            clauseg ^. typeClauseNeighbors % clauseGraph % clauseGraphSpine
        argsubstitions = map 
            (second (flip TypeVar [])) 
            clauseArgSubs
        statevarsubstitiions = map 
            (\n -> 
                ( n ^. typeClauseStateVar
                , _TypeWithArgs # 
                    ( n ^. typeClauseName
                    , TypeClauseNode n 
                    , map snd argsubstitions )
                )
            ) 
            clausegraphspine

    return ( clausestatevartype
            , map snd clauseArgSubs
            , statevarsubstitiions ++ argsubstitions)
