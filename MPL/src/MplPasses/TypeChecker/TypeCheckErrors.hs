{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module MplPasses.TypeChecker.TypeCheckErrors where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed

import Data.Foldable
import Data.Function
import Data.List

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Bool
import Control.Arrow

data TypeCheckErrors = 
    SeqTypeClauseArgsMustContainTheSameTypeVariables 
        [NonEmpty [IdentP]]
        -- list of equivalence classes of the arguments on (==)
    | ConcTypeClauseArgsMustContainTheSameTypeVariables 
        [NonEmpty ([IdentP], [IdentP])]
        -- list of equivalence classes of the arguments on (==)
        --
    | ExpectedStateVarButGot IdentP IdentP 
        -- expected, actual
    | ExpectedOppositePolarity (IdentP, Polarity)
        -- channel, polarity of channel. 

    | HCaseExpectedInputPolarityChToHaveProtocolButGotCoprotocol 
        -- channel, phrase ident
        IdentP IdentP
    | HCaseExpectedOutputPolarityChToHaveCoprotocolButGotProtocol 
        -- channel, phrase ident
        IdentP IdentP

    | HPutExpectedInputPolarityChToHaveCoprotocolButGotProtocol
        IdentP IdentP
    | HPutExpectedOutputPolarityChToHaveProtocolButGotCoprotocol
        IdentP IdentP

    | ForkExpectedDisjointChannelsButHasSharedChannels [IdentP]

    | IllegalLastCommand KeyWordNameOcc 
    | IllegalNonLastCommand KeyWordNameOcc 

  deriving Show

$(makeClassyPrisms ''TypeCheckErrors)

class TypeClauseSpineSameVarError (t :: ObjectDefnTag) where
    typeClauseSpineSameVarError :: 
        AsTypeCheckErrors e => 
        MplTypeClauseSpine MplRenamed t -> 
        [e]

instance TypeClauseSpineSameVarError (SeqObjTag t) where
    typeClauseSpineSameVarError spine = bool [] 
        [_SeqTypeClauseArgsMustContainTheSameTypeVariables # eqclasses]
        shoulderror
      where
        eqclasses :: [NonEmpty [IdentP]]
        eqclasses = NE.group 
            $ fmap (fmap (view identRIdentP) <<< view typeClauseArgs) 
            $ spine ^. typeClauseSpineClauses 

        shoulderror = length eqclasses >= 2

instance TypeClauseSpineSameVarError (ConcObjTag t) where
    typeClauseSpineSameVarError spine = bool [] 
        [_ConcTypeClauseArgsMustContainTheSameTypeVariables # eqclasses]
        shoulderror
      where
        eqclasses :: [NonEmpty ([IdentP], [IdentP])]
        eqclasses = NE.group 
            $ fmap (fmap (view identRIdentP) *** fmap (view identRIdentP)
                <<< view typeClauseArgs) 
            $ spine ^. typeClauseSpineClauses 

        shoulderror = length eqclasses >= 2

hCaseError :: 
    AsTypeCheckErrors e =>
    (IdentP, Polarity) ->
    (IdentP, ConcObjDefnTag) ->
    [e]
hCaseError (ch, Input) (ident, CoprotocolDefnTag) = 
    [_HCaseExpectedInputPolarityChToHaveProtocolButGotCoprotocol # (ch,ident) ]
hCaseError (ch, Output) (ident, ProtocolDefnTag) = 
    [_HCaseExpectedOutputPolarityChToHaveCoprotocolButGotProtocol # (ch,ident) ]
hCaseError _ _ = []

hPutError :: 
    AsTypeCheckErrors e =>
    (IdentP, Polarity) ->
    (IdentP, ConcObjDefnTag) ->
    [e]
hPutError (ch, Input) (ident, ProtocolDefnTag) = 
    [ _HPutExpectedInputPolarityChToHaveCoprotocolButGotProtocol # (ch,ident) ]
hPutError (ch, Output) (ident, CoprotocolDefnTag) = 
    [_HPutExpectedOutputPolarityChToHaveProtocolButGotCoprotocol # (ch,ident) ]
hPutError _ _ = []


forkExpectedDisjointChannelsButHasSharedChannels ::
    AsTypeCheckErrors e =>
    [IdentP] ->
    [IdentP] ->
    [e]
forkExpectedDisjointChannelsButHasSharedChannels a b =
    bool [_ForkExpectedDisjointChannelsButHasSharedChannels # common ] [] (null common)
  where
    common = a `intersect` b


{-
expectedInputPolarity ::
    AsTypeCheckErrors e =>
    (IdentP, SymEntry Polarity) -> 
    [e]
expectedInputPolarity ch@(ident, SymEntry _ Output) = [_ExpectedOppositePolarity # (ident, Output)]
expectedInputPolarity _ = []

expectedOutputPolarity ::
    AsTypeCheckErrors e =>
    (IdentP, SymEntry Polarity) -> 
    [e]
expectedOutputPolarity ch@(ident, SymEntry _ Input) = [_ExpectedOppositePolarity # (ident, Input)]
expectedOutputPolarity _ = []

-}
