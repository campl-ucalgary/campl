{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MplPasses.TypeChecker.TypeCheckErrors where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked

import MplPasses.TypeChecker.KindCheck 
import MplPasses.TypeChecker.TypeEqns 
import MplPasses.TypeChecker.TypeCheckSemanticErrors 
import MplPasses.TypeChecker.TypeCheckCallErrors 
import MplPasses.TypeChecker.TypeCheckErrorPkg 
import MplPasses.TypeChecker.TypeCheckMplTypeSub

import Data.Foldable
import Data.Function
import Data.List

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Bool
import Control.Arrow

import MplPasses.PassesErrorsPprint

data TypeCheckErrors =
    SemanticErrors TypeCheckSemanticErrors
    | CallErrors TypeCheckCallErrors
  deriving Show

$(makeClassyPrisms ''TypeCheckErrors)

instance AsTypeCheckSemanticErrors TypeCheckErrors where
    _TypeCheckSemanticErrors = _SemanticErrors

instance AsTypeCheckCallErrors TypeCheckErrors where
    _TypeCheckCallErrors = _CallErrors 

instance AsTypeUnificationError TypeCheckErrors MplTypeSub where
    _TypeUnificationError = _SemanticErrors % _TypeCheckUnificationErrors 

instance AsKindCheckErrors TypeCheckErrors where
    _KindCheckErrors = _SemanticErrors % _KindCheckErrors 

pprintTypeCheckErrors :: TypeCheckErrors -> MplDoc
pprintTypeCheckErrors = go
  where
    go :: TypeCheckErrors -> MplDoc
    go = \case
        SemanticErrors semerr -> goSemErr semerr
        CallErrors callerr -> goCallErr callerr

    goSemErr :: TypeCheckSemanticErrors -> MplDoc
    goSemErr = \case
        TypeCheckKindErrors kindcheck -> pprintKindCheckErrors kindcheck

    {-
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
    -}

    goCallErr :: TypeCheckCallErrors -> MplDoc
    goCallErr = undefined




