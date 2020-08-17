{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
module MplPasses.Renamer.RenameErrors where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed

import MplUtil.UniqueSupply

import MplPasses.Renamer.RenameSym

import Data.Foldable
import Data.Function
import Data.List

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Bool

data RenameErrors =
    OverlappingDeclarations [IdentP]
    | OutOfScope IdentP

    | SeqTypeClauseArgsMustContainTheSameTypeVariables 
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

$(makeClassyPrisms ''RenameErrors)

class OverlappingDeclarations t where
    overlappingDeclarations :: AsRenameErrors e => t -> [e]

instance Foldable t => OverlappingDeclarations (t IdentP) where
    overlappingDeclarations idents = duplicates
      where
        idents' = toList idents
        identseqclasses = 
            groupBy 
                (\a b -> a ^. name == b ^. name 
                    && a ^. namespace == b ^. namespace) 
                idents'

        duplicates = foldMap f identseqclasses
          where
            f lst | length lst >= 2 = [_OverlappingDeclarations # lst]
                  | otherwise = []

instance OverlappingDeclarations (MplTypeClauseSpine MplParsed (SeqObjTag t)) where
    overlappingDeclarations (UMplTypeClauseSpine spine) = concatMap f allargs
      where
        f args = overlappingDeclarations (args <> NE.toList allstatevars)

        allargs = fmap (view typeClauseArgs) spine 
        allstatevars = fmap (view typeClauseStateVar) spine 

instance OverlappingDeclarations (MplTypeClauseSpine MplParsed (ConcObjTag t)) where
    overlappingDeclarations (UMplTypeClauseSpine spine) = concatMap f allargs
      where
        f args = overlappingDeclarations (uncurry mappend args <> NE.toList allstatevars)

        allargs :: NonEmpty ([IdentP], [IdentP])
        allargs = fmap (view typeClauseArgs) spine 

        allstatevars :: NonEmpty IdentP
        allstatevars = fmap (view typeClauseStateVar) spine 

-- default out of scope lookup
outOfScope :: 
    AsRenameErrors e =>
    SymTab -> 
    IdentP -> 
    [e]
outOfScope symtab identp = 
    maybe [_OutOfScope # identp] (const []) 
        (lookupSym identp _Nothing symtab)

outOfScopes ::
    ( Foldable t 
    , AsRenameErrors e) =>
    SymTab -> t IdentP -> [e]
outOfScopes symtab = foldMap (outOfScope symtab)

outOfScopeWith f symtab identp = 
    maybe [_OutOfScope # identp] (const []) 
        (f identp symtab)

outOfScopesWith f symtab = 
    foldMap (outOfScopeWith f symtab)

expectedInputPolarity ::
    AsRenameErrors e =>
    (IdentP, SymEntry Polarity) -> 
    [e]
expectedInputPolarity ch@(ident, SymEntry _ Output) = [_ExpectedOppositePolarity # (ident, Output)]
expectedInputPolarity _ = []

expectedOutputPolarity ::
    AsRenameErrors e =>
    (IdentP, SymEntry Polarity) -> 
    [e]
expectedOutputPolarity ch@(ident, SymEntry _ Input) = [_ExpectedOppositePolarity # (ident, Input)]
expectedOutputPolarity _ = []


class TypeClauseSpineSameVarError (t :: ObjectDefnTag) where
    typeClauseSpineSameVarError :: 
        AsRenameErrors e => 
        MplTypeClauseSpine MplParsed t -> 
        [e]

instance TypeClauseSpineSameVarError (SeqObjTag t) where
    typeClauseSpineSameVarError spine = bool [] 
        [_SeqTypeClauseArgsMustContainTheSameTypeVariables # eqclasses]
        shoulderror
      where
        eqclasses :: [NonEmpty [IdentP]]
        eqclasses = NE.group 
            $ fmap (view typeClauseArgs) 
            $ spine ^. typeClauseSpineClauses 

        shoulderror = length eqclasses >= 2

instance TypeClauseSpineSameVarError (ConcObjTag t) where
    typeClauseSpineSameVarError spine = bool [] 
        [_ConcTypeClauseArgsMustContainTheSameTypeVariables # eqclasses]
        shoulderror
      where
        eqclasses :: [NonEmpty ([IdentP], [IdentP])]
        eqclasses = NE.group 
            $ fmap (view typeClauseArgs) 
            $ spine ^. typeClauseSpineClauses 

        shoulderror = length eqclasses >= 2

hCaseError :: 
    AsRenameErrors e =>
    (IdentP, Polarity) ->
    (IdentP, ConcObjDefnTag) ->
    [e]
hCaseError (ch, Input) (ident, CoprotocolDefnTag) = 
    [_HCaseExpectedInputPolarityChToHaveProtocolButGotCoprotocol # (ch,ident) ]
hCaseError (ch, Output) (ident, ProtocolDefnTag) = 
    [_HCaseExpectedOutputPolarityChToHaveCoprotocolButGotProtocol # (ch,ident) ]
hCaseError _ _ = []

hPutError :: 
    AsRenameErrors e =>
    (IdentP, Polarity) ->
    (IdentP, ConcObjDefnTag) ->
    [e]
hPutError (ch, Input) (ident, ProtocolDefnTag) = 
    [ _HPutExpectedInputPolarityChToHaveCoprotocolButGotProtocol # (ch,ident) ]
hPutError (ch, Output) (ident, CoprotocolDefnTag) = 
    [_HPutExpectedOutputPolarityChToHaveProtocolButGotCoprotocol # (ch,ident) ]
hPutError _ _ = []


forkExpectedDisjointChannelsButHasSharedChannels ::
    AsRenameErrors e =>
    [IdentP] ->
    [IdentP] ->
    [e]
forkExpectedDisjointChannelsButHasSharedChannels a b =
    bool [_ForkExpectedDisjointChannelsButHasSharedChannels # common ] [] (null common)
  where
    common = a `intersect` b
    

