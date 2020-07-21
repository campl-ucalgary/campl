{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module MPLPasses.TieDefnsUtils where

import Optics 
import Optics.State
import Optics.State.Operators

import Control.Applicative

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST
import MPLPasses.SymbolTable
import MPLPasses.TieDefnsTypes
import MPLPasses.TieDefnsErrors
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
    SymbolTable -> 
    Maybe (UniqueTag, TypePhraseG TaggedBnfcIdent)
lookupSeqPhrase ident ~symtable =  
    let candidates = filter ((ident ^. bnfcIdentName==) . fst) symtable
        res = helper candidates
    in res
  where
    helper ((_, SymEntry tag (SymPhrase n)):rst) 
        | objtype == CodataObj || objtype == DataObj = Just (tag, n)
      where
        objtype = n ^. 
            typePhraseContext 
            % phraseParent 
            % typeClauseNeighbors 
            % clauseGraph 
            % clauseGraphObjectType

    helper ((_, SymEntry _ _):rst) = helper rst
    helper [] = Nothing

    -- errormsg = liftToGraphErrors (_SeqPhraseNotInScope # ident)

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
    -- THE PROBLEM: the valueof the unique ID depends 
    -- in the future MUST EVALUATE THIS. Hence, to fix this,
    -- we need a way to ``split" the unique number generator
    -- so we can have 2 distinct paths of unique ids so this
    -- can be truly lazy..
    clauseargsubs <- traverse 
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
            clauseargsubs
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
            , map snd clauseargsubs
            , statevarsubstitiions ++ argsubstitions)
