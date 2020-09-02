{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MplPasses.TypeChecker.TypeCheckSemanticErrors where

import Optics

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

import Data.Maybe

data TypeCheckSemanticErrors = 
    ---- Errors from the more ``heavy lifting" algorithms
    --------------------------------
    TypeCheckKindErrors KindCheckErrors
    | TypeCheckUnificationErrors (TypeUnificationError MplTypeSub)

    -- Object definition errors ...
    --------------------------------
    | SeqTypeClauseArgsMustContainTheSameTypeVariables 
        [NonEmpty [IdentR]]
        -- list of equivalence classes of the arguments on (==)
    | ConcTypeClauseArgsMustContainTheSameTypeVariables 
        [NonEmpty ([IdentR], [IdentR])]
        -- list of equivalence classes of the arguments on (==)
        --
    | ExpectedStateVarButGot IdentR IdentR 
        -- expected, actual
 

    -- Process definition errors...
    --------------------------------
    -- | Expected polarity, channel / actual polarity
    | ExpectedPolarityButGot Polarity ChIdentR
 
    -- | channel, phrase 
    | HCaseExpectedInputPolarityChToHaveProtocolButGotCoprotocol 
        ChIdentR (MplTypePhrase MplTypeChecked (ConcObjTag CoprotocolDefnTag))
    -- | channel, phrase 
    | HCaseExpectedOutputPolarityChToHaveCoprotocolButGotProtocol 
        ChIdentR (MplTypePhrase MplTypeChecked (ConcObjTag ProtocolDefnTag))

    | HPutExpectedInputPolarityChToHaveCoprotocolButGotProtocol
        IdentR IdentR
    | HPutExpectedOutputPolarityChToHaveProtocolButGotCoprotocol
        IdentR IdentR

    | ForkExpectedDisjointChannelsButHasSharedChannels [IdentR]

    | IllegalLastCommand KeyWordNameOcc 
    | IllegalNonLastCommand KeyWordNameOcc 

    | AtLastCmdThereAreUnclosedChannels (MplCmd MplRenamed) [ChIdentR]

    -- After type checking errors
    --------------------------------
    | IllegalHigherOrderFunction ( NonEmpty (MplType MplTypeSub), MplType MplTypeSub)

  deriving Show

$(makeClassyPrisms ''TypeCheckSemanticErrors)

instance AsKindCheckErrors TypeCheckSemanticErrors where
    _KindCheckErrors = _TypeCheckKindErrors  

instance AsTypeUnificationError TypeCheckSemanticErrors  MplTypeSub where
    _TypeUnificationError = _TypeCheckUnificationErrors 

{-
hCaseError :: 
    AsTypeCheckErrors e =>
    (IdentR, Polarity) ->
    (IdentR, ConcObjDefnTag) ->
    [e]
hCaseError (ch, Input) (ident, CoprotocolDefnTag) = 
    [_HCaseExpectedInputPolarityChToHaveProtocolButGotCoprotocol # (ch,ident) ]
hCaseError (ch, Output) (ident, ProtocolDefnTag) = 
    [_HCaseExpectedOutputPolarityChToHaveCoprotocolButGotProtocol # (ch,ident) ]
hCaseError _ _ = []

hPutError :: 
    AsTypeCheckErrors e =>
    (IdentR, Polarity) ->
    (IdentR, ConcObjDefnTag) ->
    [e]
hPutError (ch, Input) (ident, ProtocolDefnTag) = 
    [ _HPutExpectedInputPolarityChToHaveCoprotocolButGotProtocol # (ch,ident) ]
hPutError (ch, Output) (ident, CoprotocolDefnTag) = 
    [_HPutExpectedOutputPolarityChToHaveProtocolButGotCoprotocol # (ch,ident) ]
hPutError _ _ = []


forkExpectedDisjointChannelsButHasSharedChannels ::
    AsTypeCheckErrors e =>
    [IdentR] ->
    [IdentR] ->
    [e]
forkExpectedDisjointChannelsButHasSharedChannels a b =
    bool [_ForkExpectedDisjointChannelsButHasSharedChannels # common ] [] (null common)
  where
    common = a `intersect` b

-}

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
