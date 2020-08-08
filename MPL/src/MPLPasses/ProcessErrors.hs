module MPLPasses.ProcessErrors where 

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
import Data.Semigroup
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
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Bifunctor as Bifunctor
import Control.Arrow
import Data.Either
import Data.Function
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

import MPLPasses.CallErrors


unclosedChannelsCheck ::
    ChCxt ->
    [TieDefnsError]
unclosedChannelsCheck [] = []
unclosedChannelsCheck as = 
    [_UnclosedChannels # map (view bnfcIdent) as]


hCaseChecks ::
    NonEmpty (TypePhraseG TaggedBnfcIdent) ->
    [TieDefnsError]
hCaseChecks phrasesg = dups ++ diffgraphs ++ nonexhaus
  where
    phraseidents = fmap (view (typePhraseName % bnfcIdent)) phrasesg 

    dups = duplicatedDeclarationsCheck $ NE.toList phraseidents

    diffgraphs = maybe [] (pure . review _HCaseDifferentClauses) 
        $ phrasesDifferentGraphsCheck
        $ NE.toList phrasesg

    nonexhaus = maybe [] (pure . review _HCaseNonExhaustive) $ nonExhaustivePhrases phrasesg


nonExhaustivePhrases phrasesg@(phraseg :| _) = bool (Just missed) Nothing $ null missed
  where
    givenphrasenames = NE.toList $ fmap (view (typePhraseName % bnfcIdent)) phrasesg
    exhaustive = map (view (typePhraseName % bnfcIdent)) $ phraseg ^. typePhraseContext % phraseParent % typeClausePhrases
    missed = exhaustive \\ givenphrasenames

