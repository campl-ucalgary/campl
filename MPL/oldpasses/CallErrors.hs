module MPLPasses.CallErrors where 

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
import Data.List.NonEmpty (NonEmpty (..))
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

validRecordPhrasesCheck ::  
    NonEmpty (TypePhraseG TaggedBnfcIdent) ->
    [TieDefnsError]
validRecordPhrasesCheck phrasesg = 
    codatadeserrors ++ sameclauses ++ duplicateddecs ++ nonexhaustive
  where
    clausesg@(focusedclauseg :| _) = fmap (view (typePhraseContext % phraseParent)) phrasesg
    
    -- checking if all the phrases are codata...
    codatadeserrors = allPhrasesAreCodataCheck (NE.toList phrasesg)
    {-foldMap f $ phrasesg
      where
        f phraseg = bool 
            [_ExpectedCodataDestructor # 
                (phraseg ^. typePhraseName 
                    % taggedBnfcIdentBnfcIdent) ] 
                        []
            $ has _CodataObj (phraseg ^. phraseGObjType) 
            -}

    -- checking if all phrases are from the same clause i.e., we should have exactly 
    -- one equivalence class
    sameclauses = bool 
            ( foldMap 
                    ( pure 
                    . review _ExpectedDestructorsFromSameClause 
                    . fmap ( view (typeClauseName % taggedBnfcIdentBnfcIdent) 
                                        *** view (typePhraseName % taggedBnfcIdentBnfcIdent)) 
                    )
                    clauseeqclasses
                )
            [] 
            (length clauseeqclasses == 1)
      where
        clauseeqclasses = NE.groupBy1 ((==) `on` view typeClauseName . fst) $ NE.zip clausesg phrasesg

    -- checking if there are no duplciated declartaions.
    -- i.e., we have equivalence classes of exactly size 1
    duplicateddecs = 
        duplicatedDeclarationsCheck 
        $ NE.toList 
        $ fmap (view (typePhraseName % taggedBnfcIdentBnfcIdent)) phrasesg
    {- Old way of testing for duplicated declarations
    duplicateddecs = map (review _DuplicatedDeclarations . NE.toList) duplicateddecs where
        duplicateddecseqclasses = NE.group1 $ fmap (view (typePhraseName % taggedBnfcIdentBnfcIdent))  phrasesg
        duplicateddecs = NE.filter ((>1) . length) duplicateddecseqclasses
        -}

    -- check if all the record phrases are present in the pattern match
    -- i.e., this is an exhaustive record
    nonexhaustive = bool [] 
        [_NonExhaustiveRecordPhrases # (map (view taggedBnfcIdentBnfcIdent) nonexhaustivephrases)] 
        (not $ null nonexhaustivephrases)
      where
        nonexhaustivephrases = exhaustivephrases \\ usedphrases
        exhaustivephrases = map (view typePhraseName) $ focusedclauseg ^. typeClausePhrases
        usedphrases       = map (view typePhraseName) $ NE.toList phrasesg 


allPhrasesAreCodataCheck ::
    [TypePhraseG TaggedBnfcIdent] ->
    [TieDefnsError] 
allPhrasesAreCodataCheck = allPhrasesAreObjType CodataObj

allPhrasesAreDataCheck ::
    [TypePhraseG TaggedBnfcIdent] ->
    [TieDefnsError] 
allPhrasesAreDataCheck = allPhrasesAreObjType DataObj

allPhrasesAreObjType ::
    ObjectType ->
    [TypePhraseG TaggedBnfcIdent] ->
    [TieDefnsError] 
allPhrasesAreObjType objtype = foldMap f 
      where
        f phraseg = bool 
            [_ExpectedCodataDestructor # 
                (phraseg ^. typePhraseName 
                    % taggedBnfcIdentBnfcIdent) ] 
                        []
            $ (phraseg ^. phraseGObjType) ==  objtype
    
duplicatedDeclarationsCheck :: 
    [BnfcIdent] ->
    [TieDefnsError]
duplicatedDeclarationsCheck decs = map (review _DuplicatedDeclarations) duplicateddecs
      where
        duplicateddecsclasses = group decs
        duplicateddecs = filter ((>1) . length) duplicateddecsclasses

phrasesDifferentGraphsCheck ::
    [TypePhraseG TaggedBnfcIdent] ->
    Maybe [[BnfcIdent]]
phrasesDifferentGraphsCheck phrasesg = 
    bool Nothing (Just $ map (map fst) identstotagseq)
        (length identstotagseq /= 1)
  where
    idents = fmap (view (typePhraseName % taggedBnfcIdentBnfcIdent)) phrasesg
    tags = fmap (view (phraseGClausesGraph % uniqueTag)) phrasesg

    identstotags = zip idents tags

    identstotagseq = groupBy ((==) `on` snd) identstotags 

nonExhaustivePhrasesFromClauses ::
    NonEmpty (TypePhraseG TaggedBnfcIdent) ->
    Maybe [BnfcIdent]
nonExhaustivePhrasesFromClauses phrasesg@(phraseg :| _) = 
    bool Nothing (Just $ missedphrases ^.. folded % taggedBnfcIdentBnfcIdent) (not $ null missedphrases)
  where
    clausegraph = phraseg ^. phraseGClausesGraph

    exhaustivephrases = clauseGraphPhrases clausegraph 
    exhaustivephrasesidents = map (view typePhraseName) $ NE.toList exhaustivephrases

    phrasesgidents = map (view typePhraseName) $ NE.toList phrasesg 

    missedphrases = exhaustivephrasesidents \\ phrasesgidents
    
foldUnfoldPhrasesFromDifferentGraphsCheck ::
    [TypePhraseG TaggedBnfcIdent] -> 
    [TieDefnsError]
foldUnfoldPhrasesFromDifferentGraphsCheck phrasesg = 
    maybe [] (pure . review _FoldUnfoldPhraseFromDifferentGraphs) $ phrasesDifferentGraphsCheck phrasesg


nonExhaustiveFoldPhrase ::
    NonEmpty (TypePhraseG TaggedBnfcIdent) ->
    [TieDefnsError]
nonExhaustiveFoldPhrase phrasesg = 
    maybe [] (pure . review _NonExhaustiveFold) $ nonExhaustivePhrasesFromClauses phrasesg


nonExhaustiveUnfoldClauses ::
    NonEmpty (TypeClauseG TaggedBnfcIdent) ->
    [TieDefnsError]
nonExhaustiveUnfoldClauses clausesg@(clauseg :| _) = 
    bool []
        [ _NonExhaustiveUnfoldClauses # 
       (missedclauses ^.. folded % taggedBnfcIdentBnfcIdent)]
        (not $ null missedclauses)
  where
    exhaustiveclauses = clauseg ^..
        typeClauseNeighbors 
        % clauseGraph 
        % clauseGraphSpine 
        % folded 
        % typeClauseName 

    clausegidents = clausesg ^.. 
        folded 
        % typeClauseName 

    missedclauses = exhaustiveclauses \\ clausegidents

validUnfoldTypePhrasesCheck ::
    NonEmpty (NonEmpty (TypePhraseG TaggedBnfcIdent)) ->
    [TieDefnsError]
validUnfoldTypePhrasesCheck phrasesg = samegraph ++ duplicatedphrases ++ nonexhaustiveclauses ++ recordchecks 
  where
    phrasesg' = NE.toList $ sconcat phrasesg

    samegraph =  
        foldUnfoldPhrasesFromDifferentGraphsCheck 
            phrasesg'
    
    nonexhaustiveclauses = nonExhaustiveUnfoldClauses
        $ phrasesg & mapped %~ view (to NE.head % typePhraseContext % phraseParent)

    -- just check if all the phraes are duplciated..
    -- With this, in combination with exhaustiveclauses,
    -- we know that this will include all clauses
    -- and phrases... TODO, shoudl really just check for duplciated clauses here... 
    duplicatedphrases = duplicatedDeclarationsCheck
        $ phrasesg' & mapped %~ view (typePhraseName % taggedBnfcIdentBnfcIdent)

    recordchecks = sconcat $ phrasesg & mapped %~ validRecordPhrasesCheck 



validFoldTypePhrasesCheck ::
    NonEmpty (TypePhraseG TaggedBnfcIdent) ->
    [TieDefnsError]
validFoldTypePhrasesCheck phrasesg = duplicateddecs ++ samegraphs ++ nonexhaustive ++ alldata
  where
    duplicateddecs = duplicatedDeclarationsCheck (map (view (typePhraseName % taggedBnfcIdentBnfcIdent)) 
            $ NE.toList phrasesg)

    samegraphs = foldUnfoldPhrasesFromDifferentGraphsCheck $ NE.toList phrasesg

    alldata = allPhrasesAreDataCheck $ NE.toList phrasesg

    nonexhaustive = nonExhaustiveFoldPhrase  phrasesg



