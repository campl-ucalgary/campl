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

data TypeCheckSemanticErrors = 
    ---- Errors from the more ``heavy lifting" algorithms
    --------------------------------
    TypeCheckKindErrors KindCheckErrors
    | TypeCheckUnificationErrors (TypeUnificationError MplTypeSub)

    -- Codata record construction errors ...
    ------------------------------------
    | RecordConstructionErrorGotPhrasesButExpected
        (NonEmpty (IdP MplTypeChecked)) [MplTypePhrase MplTypeChecked (SeqObjTag CodataDefnTag)]


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
    | ExpectedOppositePolarity (IdentR, Polarity)
        -- channel, polarity of channel. 
    | HCaseExpectedInputPolarityChToHaveProtocolButGotCoprotocol 
        -- channel, phrase ident
        IdentR IdentR
    | HCaseExpectedOutputPolarityChToHaveCoprotocolButGotProtocol 
        -- channel, phrase ident
        IdentR IdentR

    | HPutExpectedInputPolarityChToHaveCoprotocolButGotProtocol
        IdentR IdentR
    | HPutExpectedOutputPolarityChToHaveProtocolButGotCoprotocol
        IdentR IdentR

    | ForkExpectedDisjointChannelsButHasSharedChannels [IdentR]

    | IllegalLastCommand KeyWordNameOcc 
    | IllegalNonLastCommand KeyWordNameOcc 

    | IllegalHigherOrderFunction ( NonEmpty (MplType MplTypeSub), MplType MplTypeSub)

    -- 
    | Huhh

    -- Process definition errors...
    --------------------------------
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


expectedInputPolarity ::
    AsTypeCheckErrors e =>
    (IdentR, SymEntry Polarity) -> 
    [e]
expectedInputPolarity ch@(ident, SymEntry _ Output) = [_ExpectedOppositePolarity # (ident, Output)]
expectedInputPolarity _ = []

expectedOutputPolarity ::
    AsTypeCheckErrors e =>
    (IdentR, SymEntry Polarity) -> 
    [e]
expectedOutputPolarity ch@(ident, SymEntry _ Input) = [_ExpectedOppositePolarity # (ident, Input)]
expectedOutputPolarity _ = []

-}
