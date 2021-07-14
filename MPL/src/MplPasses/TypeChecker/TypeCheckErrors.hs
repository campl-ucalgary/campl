{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Arrow hiding ((<+>))

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
        -- Type check errors
        TypeCheckKindErrors kindcheck -> pprintKindCheckErrors kindcheck
        TypeCheckUnificationErrors uniferror -> pprintTypeUnificationError uniferror

        -- Object definition errors
        SeqTypeClauseArgsMustContainTheSameTypeVariables eqclasses -> fold
            [ pretty "Sequential type declaration must all contain the same argument type variables." ]
        {- TODO: SeqTypeClauseArgsMustContainTheSameTypeVariables [NonEmpty [IdP MplRenamed]] 
         list of equivalence classes of the arguments on (==) -}
        ConcTypeClauseArgsMustContainTheSameTypeVariables eqclasses -> fold
            [ pretty "Concurrent type declaration must all contain the same argument type variables." ]
        {- TODO: ConcTypeClauseArgsMustContainTheSameTypeVariables [NonEmpty ([IdP MplRenamed], [IdP MplRenamed])]
        list of equivalence classes of the arguments on (==) -}
        ExpectedStateVarButGot expected actual -> fold
            [ pretty "Expected the state variable"
            , codeblock
                $ (pprintParsed :: MplType MplRenamed -> String)
                $ TypeVar () expected
            , pretty "but got"
            , codeblock
                $ (pprintParsed :: MplType MplRenamed -> String)
                $ TypeVar () actual
            , pretty "at"
                <+> actual ^. location % to pprintLoc 
            ]

        -- Expression errors
        ExpectedFoldPhraseToBeEitherButGot expectedphrases got -> fold
            [ pretty "Expected `fold' phrases to have one of"
            , vsep $ map 
                ( codeblock
                . (pprintParsed :: MplType MplRenamed -> String)
                . TypeVar () 
                ) expectedphrases
            , pretty "but got"
            , codeblock
                . (pprintParsed :: MplType MplRenamed -> String)
                . TypeVar () $ got
            , pretty "at"
                <+> got ^. location % to pprintLoc 
            ]
        ExpectedUnfoldPhraseToBeEitherButGot expectedphrases got -> fold
            [ pretty "Expected `unfold' phrases to have one of"
            , vsep $ map 
                ( codeblock
                . (pprintParsed :: MplType MplRenamed -> String)
                . TypeVar () 
                ) expectedphrases
            , pretty "but got"
            , codeblock
                . (pprintParsed :: MplType MplRenamed -> String)
                . TypeVar () $ got
            , pretty "at"
                <+> got ^. location % to pprintLoc 
            ]
        -- Process definition errors.
        ExpectedPolarityButGot expected chp -> fold
            [ pretty "Expected polarity of"
                <+> pretty (show expected)
                <+> pretty "but got the following channel of opposite polarity"
            , codeblock $ pprintParsed chp
            , pretty "at"
                <+> chp  ^. location % to pprintLoc 
            ]
        -- TODO: actually use the phrase?
        HCaseExpectedInputPolarityChToHaveProtocolButGotCoprotocol chp phrase -> fold
            [ pretty "Illegal `hcase' command. Expected input polarity channel from "
            , codeblock
                $ pprintParsed chp
            , pretty "at"
                <+> chp  ^. location % to pprintLoc 
                <+> pretty "to have a `protocol' but got a `coprotocol' instead."
            ]
        HCaseExpectedOutputPolarityChToHaveCoprotocolButGotProtocol chp phrase -> fold
            [ pretty "Illegal `hcase' command. Expected output polarity channel from"
            , codeblock
                $ pprintParsed chp
            , pretty "at"
                <+> chp  ^. location % to pprintLoc 
                <+> pretty "to have a `coprotocol' but got a `protocol' instead."
            ]

        HPutExpectedInputPolarityChToHaveCoprotocolButGotProtocol hput chp phrase -> fold
            [ pretty "Illegal `"
                <> pretty 
                    ( hput ^. 
                        (coerced :: Iso' KeyWordNameOcc NameOcc) 
                        % name 
                        % (coerced :: Iso' Name String)
                        ) 
                <> pretty "' command at"
                <+> hput ^. location % to pprintLoc
                <> pretty "."
                <+> pretty "Expected input polarity channel"
            , codeblock
                $ pprintParsed chp
            , pretty "at"
                <+> chp  ^. location % to pprintLoc 
                <+> pretty "to have a `coprotocol' but got a `protocol' instead."
            ]
        HPutExpectedOutputPolarityChToHaveProtocolButGotCoprotocol hput chp phrase -> fold
            [ pretty "Illegal `"
                <> pretty 
                    ( hput ^. 
                        (coerced :: Iso' KeyWordNameOcc NameOcc) 
                        % name 
                        % (coerced :: Iso' Name String)
                        ) 
                <> pretty "' command at"
                <+> hput ^. location % to pprintLoc
                <> pretty "."
                <+> pretty "Expected output polarity channel"
            , codeblock
                $ pprintParsed chp
            , pretty "at"
                <+> chp  ^. location % to pprintLoc 
                <+> pretty "to have a `protocol' but got a `coprotocol' instead."
            ]
        ForkExpectedDisjointChannelsButHasSharedChannels fork chs -> fold
            [ pretty "Illegal `"
                <> pretty 
                    ( fork ^. 
                        (coerced :: Iso' KeyWordNameOcc NameOcc) 
                        % name 
                        % (coerced :: Iso' Name String)
                        ) 
                <> pretty "' command at"
                <+> fork ^. location % to pprintLoc
                <> pretty "."
                <+> pretty "Channels in a `fork' command must be disjoint but they are not."
            ]

        ForkHasChannelsInScopeButContextsAreNonExhaustiveWith 
            fork scoped (cxt0, cxt1) nonexhaustive -> fold
            [ pretty "Illegal `"
                <> pretty 
                    ( fork ^. 
                        (coerced :: Iso' KeyWordNameOcc NameOcc) 
                        % name 
                        % (coerced :: Iso' Name String)
                        ) 
                <> pretty "' command at"
                <+> fork ^. location % to pprintLoc
                <> pretty "."
                <+> pretty "A `fork' command must split all channels in scope."
                <+> pretty "There are channels"
                <+> hsep (map (pretty . pprintParsed) scoped)
                <+> pretty "in scope, but are nonexhaustive with"
                <+> hsep (map (pretty . pprintParsed) nonexhaustive)
            ]
        IllegalIdGotChannelsOfTheSamePolarityButIdNeedsDifferentPolarity idop ch0 ch1 -> fold
            [ pretty "Illegal `"
                <> pretty 
                    ( idop ^. 
                        (coerced :: Iso' KeyWordNameOcc NameOcc) 
                        % name 
                        % (coerced :: Iso' Name String)
                        ) 
                <> pretty "' command at"
                <+> idop ^. location % to pprintLoc
                <> pretty "."
                <+> pretty "Channels"
                <+> hsep (map (pretty . pprintParsed) [ch0, ch1])
                <+> pretty "have the same polarity, but must have different polarity."
            ]
        IllegalIdNegGotChannelsOfDifferentPolarityButIdNegNeedsTheSamePolarity neg ch0 ch1 -> fold
            [ pretty "Illegal `"
                <> pretty 
                    ( neg ^. 
                        (coerced :: Iso' KeyWordNameOcc NameOcc) 
                        % name 
                        % (coerced :: Iso' Name String)
                        ) 
                <> pretty "' command at"
                <+> neg ^. location % to pprintLoc
                <> pretty "."
                <+> pretty "Channels"
                <+> hsep (map (pretty . pprintParsed) [ch0, ch1])
                <+> pretty "have different polarity, but must have the same polarity."
            ]
        {- IllegalRaceAgainstDifferentPolarities KeyWordNameOcc [ChP MplRenamed] [ChP MplRenamed] -}
        -- input polarities, output polarities
        -- We don't test this because this will result in a type error anyways, but
        -- Dr.Cockett mentioned we should? Ask him about this later....
        IllegalRaceAgainstDifferentPolarities _ _ _ -> error "Illegal race against different polarities"

        ExpectedVariablesToBeInADifferentPlugPhraseButGotIn chs (lplug, rplug) -> fold
            [ pretty "Illegal `plug' commmand."
                <+> pretty "Channels may not occur in both of the left and right side of the plug phrase."
            ]

        ExpectedAtMostTwoOccurencesOfAChannelInAPlugPhraseButGot chs -> fold
            [ pretty "Illegal `plug' commmand."
                <+> pretty "Expected at most two occurences of a channel, but got channels"
                <> line
                <> indent 2 (pprintChs chs)
            ]
        ExpectedVariablesToBeOfOppositePolarityInAPlugPhraseButGot chs -> fold
            [ pretty "Illegal `plug' command."
                <+> pretty "All plugged channels must be of opposite polarity, but the following channels have the same polarity."
                <> line
                <+> indent 2 (pprintChs chs)
            ]

        ExpectedAtMostOnePluggedChannelToBeConnectingPlugPhrases intersected -> fold
            [ pretty "Illegal `plug' command."
                <+> pretty "Plug phrases must be connected by at most one plugged channel, but the following plugged channels violate this condition."
                <> line
                <+> indent 2 (pprintChs intersected)
            ]

        IllegalCycleInPlugPhrase phrases -> fold
            [ pretty "Illegal `plug' command."
            , pretty "Cycles in a plug command are not allowed."
            ]

        UnreachablePhrasesInPlugPhrase phrases0 phrases1 -> fold
            [ pretty "Illegal `plug' command."
            , pretty "There are unreachable plug phrases which are not allowed."
            ]
            

        IllegalLastCommand cmd ->  hsep
            [ pprintIllegalCmd cmd
            , pretty "This command cannot be the last command in a command block."
            ]

        IllegalNonLastCommand cmd -> hsep
            [ pprintIllegalCmd cmd
            , pretty "This command cannot be not the last command in a command block."
            ]
        AtLastCmdThereAreUnclosedChannels cmd chs -> fold
            [ pretty "At the last command "
            , codeblock $ pprintParsed cmd 
            , pretty "there are illegal unclosed channels as follows."
            , indent 2 $ pprintChs chs
            ]

        IllegalHigherOrderFunction ty -> fold
            [ pretty "Illegal higher order function of"
            , pretty 
                $ (pprintParsed :: MplType MplTypeSub -> String)
                $ TypeBuiltIn 
                $ uncurry (TypeSeqArrF Nothing) ty
            ]
      where
        pprintChs chs = vsep 
            $ map (\ch -> hsep
                        [ pretty "`" <> pretty (pprintParsed ch) <> pretty "'"
                        , pretty "at" 
                        , ch ^. location % to pprintLoc 
                        ]
                    ) chs

        pprintIllegalCmd cmd =
            pretty "Illegal `"
                <> pretty 
                    ( cmd ^. 
                        (coerced :: Iso' KeyWordNameOcc NameOcc) 
                        % name 
                        % (coerced :: Iso' Name String)
                        ) 
                <> pretty "' command at"
                <+> cmd ^. location % to pprintLoc
                <> pretty "."

    goCallErr :: TypeCheckCallErrors -> MplDoc
    goCallErr err = case err of 
        CannotCallTerm idp -> hsep
            [ pretty "Cannot call"
            , idp ^. identRIdentP % to pprintIdentPWithLoc 
            , pretty "(most likely because the term is invalid)."
            ]
        CannotCallCh chp -> hsep
            [ pretty "Cannot call"
            , chp ^. chIdentRIdentR % identRIdentP % to pprintIdentPWithLoc 
            , pretty "(most likely because the channel is invalid)."
            ]
        CannotCallTypeCts tpp -> hsep
            [ pretty "Cannot call"
            , tpp ^. identRIdentP % to pprintIdentPWithLoc 
            , pretty "(most likely because the type constructor is invalid)."
            ]

        -- TODO: perhaps show the phrase to show what the user may write.
        IllegalPattDataCallGotCodataInstead patt phrase -> fold
            [ pretty "Illegal pattern data call with unexpected codata in"
            , codeblock $ pprintParsed patt
            ]

        IllegalPattCodataCallGotDataInstead patt phrase -> fold
            [ pretty "Illegal pattern codata call with unexpected data in"
            , codeblock $ pprintParsed patt
            ]
        IllegalExprDataCallGotCodataInstead expr phrase -> fold
            [ pretty "Illegal expression data call with unexpected codata in"
            , codeblock $ pprintParsed expr
            ]
        IllegalExprCodataCallGotDataInstead expr phrase -> fold
            [ pretty "Illegal expression codata call with unexpected data in"
            , codeblock $ pprintParsed expr
            ]

        ExpectedPattCodataCallButGotADataCall phrase patt -> fold
            [ pretty "Expected pattern codata call, but got a data call in"
            , codeblock $ pprintParsed patt
            ]
