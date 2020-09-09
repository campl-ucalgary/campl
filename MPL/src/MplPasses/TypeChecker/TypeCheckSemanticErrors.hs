{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MplPasses.TypeChecker.TypeCheckSemanticErrors where

import Optics
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked

import MplPasses.TypeChecker.KindCheck 
import MplPasses.TypeChecker.TypeEqns 
import MplPasses.TypeChecker.TypeCheckMplTypeSub 

import Data.Foldable
import Data.Function
import Data.List

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Bool
import Control.Arrow

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Monad.State
import Control.Monad.Writer

import Data.Traversable

import Debug.Trace

import Data.Maybe

data TypeCheckSemanticErrors = 
    ---- Errors from the more ``heavy lifting" algorithms
    --------------------------------
    TypeCheckKindErrors KindCheckErrors
    | TypeCheckUnificationErrors (TypeUnificationError MplTypeSub)

    -- Object definition errors ...
    --------------------------------
    | SeqTypeClauseArgsMustContainTheSameTypeVariables 
        [NonEmpty [IdP MplRenamed]]
        -- list of equivalence classes of the arguments on (==)
    | ConcTypeClauseArgsMustContainTheSameTypeVariables 
        [NonEmpty ([IdP MplRenamed], [IdP MplRenamed])]
        -- list of equivalence classes of the arguments on (==)
        --
    | ExpectedStateVarButGot (IdP MplRenamed) (IdP MplRenamed)
        -- expected, actual
 
    -- Expression errors 
    --------------------------------
    -- | fold phrases must all be within the same data graph as the FIRST 
    -- phrase given...
    | ExpectedFoldPhraseToBeEitherButGot [IdP MplRenamed] (IdP MplRenamed) 
    | ExpectedUnfoldPhraseToBeEitherButGot [IdP MplRenamed] (IdP MplRenamed) 

    -- Process definition errors...
    --------------------------------
    -- | Expected polarity, channel / actual polarity
    | ExpectedPolarityButGot Polarity (ChP MplRenamed)
 
    -- | channel, phrase 
    | HCaseExpectedInputPolarityChToHaveProtocolButGotCoprotocol 
        (ChP MplRenamed) (MplTypePhrase MplTypeChecked (ConcObjTag CoprotocolDefnTag))
    -- | channel, phrase 
    | HCaseExpectedOutputPolarityChToHaveCoprotocolButGotProtocol 
        (ChP MplRenamed) (MplTypePhrase MplTypeChecked (ConcObjTag ProtocolDefnTag))

    | HPutExpectedInputPolarityChToHaveCoprotocolButGotProtocol
        KeyWordNameOcc (ChP MplRenamed) (MplTypePhrase MplTypeChecked (ConcObjTag ProtocolDefnTag))
    | HPutExpectedOutputPolarityChToHaveProtocolButGotCoprotocol
        KeyWordNameOcc (ChP MplRenamed) (MplTypePhrase MplTypeChecked (ConcObjTag CoprotocolDefnTag))

    | ForkExpectedDisjointChannelsButHasSharedChannels KeyWordNameOcc [ChP MplRenamed]
    | ForkHasChannelsInScopeButContextsAreNonExhaustiveWith 
        KeyWordNameOcc [ChP MplRenamed] 
            ([ChP MplRenamed], [ChP MplRenamed]) 
            [ChP MplRenamed]

    | IllegalIdGotChannelsOfTheSamePolarityButIdNeedsDifferentPolarity
        KeyWordNameOcc (ChP MplRenamed) (ChP MplRenamed)
    | IllegalIdNegGotChannelsOfDifferentPolarityButIdNegNeedsTheSamePolarity
        KeyWordNameOcc (ChP MplRenamed) (ChP MplRenamed)

    -- | input polarities, output polarities
    -- We don't test this because this will result in a type error anyways, but
    -- Dr.Cockett mentioned we should? Ask him about this later....
    | IllegalRaceAgainstDifferentPolarities KeyWordNameOcc [ChP MplRenamed] [ChP MplRenamed]

    -- | Cut condition failures..
    -- (c1), (c2), (c3), cycle condition
    | ExpectedAtMostTwoOccurencesOfAChannelInAPlugPhraseButGot [ChIdentR]
    | ExpectedVariablesToBeOfOppositePolarityInAPlugPhraseButGot [ChIdentR] 
    | ExpectedVariablesToBeInADifferentPlugPhraseButGotIn [ChIdentR] ([ChIdentR], [ChIdentR])

    | IllegalCycleInPlugPhrase [([ChIdentR], [ChIdentR])]
    | UnreachablePhrasesInPlugPhrase [([ChIdentR], [ChIdentR])] [([ChIdentR], [ChIdentR])]

    | IllegalLastCommand KeyWordNameOcc 
    | IllegalNonLastCommand KeyWordNameOcc 


    | AtLastCmdThereAreUnclosedChannels (MplCmd MplRenamed) [ChP MplRenamed]

    -- After type checking errors
    --------------------------------
    | IllegalHigherOrderFunction ( NonEmpty (MplType MplTypeSub), MplType MplTypeSub)

  deriving Show

$(makeClassyPrisms ''TypeCheckSemanticErrors)

instance AsKindCheckErrors TypeCheckSemanticErrors where
    _KindCheckErrors = _TypeCheckKindErrors  

instance AsTypeUnificationError TypeCheckSemanticErrors  MplTypeSub where
    _TypeUnificationError = _TypeCheckUnificationErrors 

expectedOutputPolarity ::
    AsTypeCheckSemanticErrors e =>
    ChIdentR -> [e]
expectedOutputPolarity ch = maybeToList $ 
    _ExpectedPolarityButGot # (Output, ch) <$ ch ^? polarity % _Input

expectedInputPolarity ::
    AsTypeCheckSemanticErrors e =>
    ChIdentR -> [e]
expectedInputPolarity ch = maybeToList $ 
    _ExpectedPolarityButGot # (Input, ch) <$ ch ^? polarity % _Output



{-
 - Tests for the following conditions:
 -      (c1) at most 2 occurences of each variable, 
 -      (c2) each variable must be of opposite polarity in each of the plug phrases
 -      (c3) each variable must be in a different phrase
 -}
cutConditions :: 
    forall e. 
    ( AsTypeCheckSemanticErrors e ) => 
    [([ChIdentR], [ChIdentR])] ->
    [e]
cutConditions plugphrases = 
    c1c2 plugphrases <> concatMap c3 plugphrases
  where
    c1c2 = concatMap g . toListOf folded . flip execState Map.empty . traverse f 

    -- create a map of: channel --> (occurences in input phrase, occurences in output phrase)
    f :: ( MonadState (Map UniqueTag ([ChIdentR], [ChIdentR])) m ) => 
        ([ChIdentR], [ChIdentR]) -> 
        m ()
    f (ins, outs) = do
        for_ ins $ \ch -> at (ch ^. uniqueTag) %= Just . maybe ([ch], []) (over _1 (ch:))
        for_ outs $ \ch -> at (ch ^. uniqueTag) %= Just . maybe ([], [ch]) (over _2 (ch:))

    g :: ([ChIdentR], [ChIdentR]) -> [e]
    g res = fold [c1 res, c2 res]
    
    c1 inputsoutputs 
        | uncurry (+) (over each length inputsoutputs) > 2 =
            [ _ExpectedAtMostTwoOccurencesOfAChannelInAPlugPhraseButGot # uncurry (<>) inputsoutputs ]
        | otherwise = [] 

    c2 (as@(_:_:_), []) = [ _ExpectedVariablesToBeOfOppositePolarityInAPlugPhraseButGot # as ]
    c2 ([], bs@(_:_:_)) = [ _ExpectedVariablesToBeOfOppositePolarityInAPlugPhraseButGot # bs ]
    c2 _ = []

    c3 phrase@(ins, outs) = 
        let common = ins `intersect` outs
        in bool [_ExpectedVariablesToBeInADifferentPlugPhraseButGotIn # (common, phrase)]
            [] $ null common

type FocusedPhraseMap = 
    ( Map UniqueTag (ChIdentR, [([ChIdentR], [ChIdentR])])
    , Map UniqueTag (ChIdentR, [([ChIdentR], [ChIdentR])]) 
    )

data CutCyclesEnv = CutCyclesEnv {
    -- | Unique tag to ChIdentR and number of times it occured
    _focusedPhrase :: FocusedPhraseMap
    , _unfocusedPhrases :: [([ChIdentR], [ChIdentR])]
}

$(makeLenses ''CutCyclesEnv)

{- Tests if there is a cycle in the plug and if the plug
 - graph is fully connected...
 -
 - Might be a good idea to generalize this later? Really bound to just
 - this specific kind of graph... Perhaps look into operations in Data.Graph from 
 - containers.
 -}
cutCycles :: 
    forall e. 
    ( AsTypeCheckSemanticErrors e ) => 
    -- | phrases
    NonEmpty ([ChIdentR], [ChIdentR]) ->
    [e]
cutCycles (start@(startins,startouts) :| phrases) = execWriter 
    $ flip evalStateT (CutCyclesEnv initfocused phrases) 
    $ loop
  where
    initfocused = (Map.fromList $ map f startins, Map.fromList $ map f startouts)
      where
        f ch = (ch ^. uniqueTag, (ch, [start]))

    allphrases = start:phrases

    loop :: ( MonadWriter [e] m, MonadState CutCyclesEnv m ) => m ()
    loop = do
        focused@(infocus, outfocus) <- guse focusedPhrase
        unfocused <- guse unfocusedPhrases
        let common = Map.keys $ uncurry Map.intersection focused
            cycles = map (\k -> fromJust (infocus ^? at k % _Just % _2 ) 
                <> fromJust (outfocus ^? at k % _Just % _2 % to reverse)) common
            
        tell $ bool (map (review _IllegalCycleInPlugPhrase) cycles) [] $ has _Empty common

        let infocuschs = infocus ^.. folded % _1
            infindsres = map (flip findAndRemoveNodeOutput unfocused) infocuschs
            inres = getFirst $ foldMap First infindsres

            outfocuschs = outfocus ^.. folded % _1
            outfindsres = map (flip findAndRemoveNodeInput unfocused) outfocuschs
            outres = getFirst $ foldMap First outfindsres

        joinEdges inres outres
    
    joinEdges :: ( MonadWriter [e] m, MonadState CutCyclesEnv m ) => 
        Maybe (([ChIdentR], [ChIdentR]), [([ChIdentR], [ChIdentR])]) -> 
        Maybe (([ChIdentR], [ChIdentR]), [([ChIdentR], [ChIdentR])]) -> 
        m ()
    joinEdges Nothing Nothing = do
        unfocused <- guse unfocusedPhrases
        tell $ bool [] [ _UnreachablePhrasesInPlugPhrase # (allphrases \\ unfocused, unfocused) ] 
             $ hasn't _Empty unfocused
        return ()

    -- input sub found
    joinEdges (Just (phrase@(phraseins, phraseouts), nunfocused)) _ = do
        focusedins <- guses (focusedPhrase % _1) Map.keys
        let pluggedintersect = filter ((`elem` focusedins) . view uniqueTag) phraseouts

        for pluggedintersect $ \pluggedch -> do
            focusedPhrase % _1 % at (pluggedch ^. uniqueTag) .= Nothing

        let phraseouts' = phraseouts ^. to (over equality (filter (`notElem`pluggedintersect)))
            phraseins' = phraseins 
        for phraseouts' $ \outch -> do
            focusedPhrase % _2 % at (outch ^. uniqueTag) %= 
                Just . maybe ((outch, [phrase])) (const outch *** (phrase:)) 
        for phraseins' $ \inch -> do
            focusedPhrase % _1 % at (inch ^. uniqueTag) %= 
                Just . maybe ((inch, [phrase])) (const inch *** (phrase:)) 

        unfocusedPhrases .= nunfocused

        loop

    -- output sub found
    -- (duplciated code...)
    joinEdges _ (Just (phrase@(phraseins, phraseouts), nunfocused)) = do
        focusedouts <- guses (focusedPhrase % _2) Map.keys
        let pluggedintersect = filter ((`elem` focusedouts) . view uniqueTag) phraseins

        for pluggedintersect $ \pluggedch -> do
            focusedPhrase % _2 % at (pluggedch ^. uniqueTag) .= Nothing

        let phraseouts' = phraseouts 
            phraseins' = phraseins ^. to (over equality (filter (`notElem`pluggedintersect)))
        for phraseouts' $ \outch -> do
            focusedPhrase % _2 % at (outch ^. uniqueTag) %= 
                Just . maybe ((outch, [phrase])) (const outch *** (phrase:)) 
        for phraseins' $ \inch -> do
            focusedPhrase % _1 % at (inch ^. uniqueTag) %= 
                Just . maybe ((inch, [phrase])) (const inch *** (phrase:)) 

        unfocusedPhrases .= nunfocused

        loop

    findAndRemoveNodeInput ::
        ChIdentR -> 
        [([ChIdentR], [ChIdentR]) ] -> 
        Maybe ( ([ChIdentR], [ChIdentR]), [([ChIdentR], [ChIdentR])])
    findAndRemoveNodeInput = findAndRemoveNodeBy (\ch -> elem ch . fst)

    findAndRemoveNodeOutput ::
        ChIdentR -> 
        [([ChIdentR], [ChIdentR]) ] -> 
        Maybe (([ChIdentR], [ChIdentR]), [([ChIdentR], [ChIdentR])])
    findAndRemoveNodeOutput = findAndRemoveNodeBy (\ch -> elem ch . snd)

    findAndRemoveNodeBy :: 
        ( ChIdentR -> ([ChIdentR], [ChIdentR]) -> Bool) ->
        ChIdentR -> 
        [([ChIdentR], [ChIdentR]) ] -> 
        Maybe (([ChIdentR], [ChIdentR]), [([ChIdentR], [ChIdentR])])
    findAndRemoveNodeBy p ch = f
      where
        f [] = Nothing
        f (phrase:phrases) 
            | p ch phrase = Just (phrase, phrases)
            -- | ch `elem` uncurry (<>) phrase = Just (phrase, phrases)
            | otherwise = second (phrase:) <$> f phrases
