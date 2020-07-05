{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module MPLAST.MPLASTTranslateErrors where

import Optics

import Data.Function
import Data.Tuple

import MPLAST.MPLProg
import MPLAST.MPLProgI
import MPLAST.MPLTypeAST

import Data.List.NonEmpty

import Language.AbsMPL as B

data TranslateBnfcErrors =
    IllegalGetPut BnfcIdent
    | IllegalDataDeclaration { _translateBnfcErrorStateVar :: Type BnfcIdent BnfcIdent
                                , _translateBnfcErrorDecName :: Type BnfcIdent BnfcIdent }
    | IllegalCodataDeclaration { _translateBnfcErrorStateVar :: Type BnfcIdent BnfcIdent
                                , _translateBnfcErrorDecName :: Type BnfcIdent BnfcIdent }
    | IllegalProtocolDeclaration { _translateBnfcErrorStateVar :: Type BnfcIdent BnfcIdent
                                , _translateBnfcErrorDecName :: Type BnfcIdent BnfcIdent }
    | IllegalCoprotocolDeclaration { _translateBnfcErrorStateVar :: Type BnfcIdent BnfcIdent
                                , _translateBnfcErrorDecName :: Type BnfcIdent BnfcIdent }
    | IllegalNonTypeVar { _translateBnfcErrorIllegalNonTypeVar :: Type BnfcIdent BnfcIdent }

    | IllegalSplit { _translateBnfcErrorIllegalSplit :: [BnfcIdent] }
    | IllegalFork { _translateBnfcErrorIllegalSplit :: [BnfcIdent] }

$(makeClassyPrisms ''TranslateBnfcErrors)

-- useful utilities too 
uIdentBnfcIdentGetter :: Getter UIdent BnfcIdent
uIdentBnfcIdentGetter = to get
  where
    get :: UIdent -> BnfcIdent
    get name = name ^. coercedTo @((Int,Int), String) % swapped 
                    & _2 %~ id

pIdentBnfcIdentGetter :: Getter PIdent BnfcIdent
pIdentBnfcIdentGetter = to get
  where
    get :: PIdent -> BnfcIdent
    get name = name ^. coercedTo @((Int,Int), String) % swapped 
                    & _2 %~ id 

pIntegerGetter :: Getter PInteger (BnfcIdent, Int)
pIntegerGetter = to get
  where
    get :: PInteger -> (BnfcIdent, Int)
    get (PInteger ident) = (swap ident, read (ident ^. _2))

{-
pCharGetter :: Getter PInteger (BnfcIdent, Int)
pCharGetter = to get
  where
    get :: PInteger -> (BnfcIdent, Int)
    get (PInteger ident) = (swap ident, read (ident ^. _2))
    -}

lBracketBnfcIdentGetter :: Getter LBracket BnfcIdent
lBracketBnfcIdentGetter = to get
  where
    get :: LBracket -> BnfcIdent
    get name = name ^. coercedTo @((Int,Int), String) % swapped 
                    & _2 %~ id 

lSquareBracketBnfcIdentGetter :: Getter LSquareBracket BnfcIdent
lSquareBracketBnfcIdentGetter = to get
  where
    get :: LSquareBracket -> BnfcIdent
    get name = name ^. coercedTo @((Int,Int), String) % swapped 
                    & _2 %~ id 

colonBnfcIdentGetter :: Getter Colon BnfcIdent
colonBnfcIdentGetter = to get
  where
    get :: Colon -> BnfcIdent
    get name = name ^. coercedTo @((Int,Int), String) % swapped 
                    & _2 %~ id 

nullPatternBnfcIdentGetter :: Getter NullPattern BnfcIdent
nullPatternBnfcIdentGetter = to get
  where
    get :: NullPattern -> BnfcIdent
    get name = name ^. coercedTo @((Int,Int), String) % swapped 
                    & _2 %~ id 
