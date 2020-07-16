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
    | IllegalDataDeclaration TranslateBnfcErrors
    | IllegalCodataDeclaration TranslateBnfcErrors
    | IllegalProtocolDeclaration TranslateBnfcErrors
    | IllegalCoprotocolDeclaration TranslateBnfcErrors
    | IllegalTypeName { _translateBnfcErrorIllegalTypeName :: TypeIBnfc }
    | IllegalNonTypeVar { _translateBnfcErrorIllegalNonTypeVar :: TypeIBnfc }

    | IllegalCasePattern { _translateBnfcErrorIllegalCase :: [PatternIBnfc] }

    | IllegalSplit { _translateBnfcErrorIllegalSplit :: [BnfcIdent] }
    | IllegalFork { _translateBnfcErrorIllegalFork :: [BnfcIdent] }

  deriving (Show, Eq, Read)

$(makeClassyPrisms ''TranslateBnfcErrors)

-- useful utilities too 
uIdentBnfcIdentGetter :: Getter UIdent BnfcIdent
uIdentBnfcIdentGetter = to get
  where
    get :: UIdent -> BnfcIdent
    get name = BnfcIdent (name ^. 
        coercedTo @((Int,Int), String) 
        % swapped)

pIdentBnfcIdentGetter :: Getter PIdent BnfcIdent
pIdentBnfcIdentGetter = to get
  where
    get :: PIdent -> BnfcIdent
    get name = BnfcIdent (name ^. 
        coercedTo @((Int,Int), String) 
        % swapped)

pIntegerGetter :: Getter PInteger (BnfcIdent, Int)
pIntegerGetter = to get
  where
    get :: PInteger -> (BnfcIdent, Int)
    get (PInteger ident) = 
        ( BnfcIdent (view (swapped % coerced) ident)
        , read (ident ^. _2))

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
    get name = BnfcIdent (name ^. 
        coercedTo @((Int,Int), String) 
        % swapped)

lSquareBracketBnfcIdentGetter :: Getter LSquareBracket BnfcIdent
lSquareBracketBnfcIdentGetter = to get
  where
    get :: LSquareBracket -> BnfcIdent
    get name = BnfcIdent (name ^. 
        coercedTo @((Int,Int), String) 
        % swapped)

colonBnfcIdentGetter :: Getter Colon BnfcIdent
colonBnfcIdentGetter = to get
  where
    get :: Colon -> BnfcIdent
    get name = BnfcIdent (name ^. 
        coercedTo @((Int,Int), String) 
        % swapped)
        

nullPatternBnfcIdentGetter :: Getter NullPattern BnfcIdent
nullPatternBnfcIdentGetter = to get
  where
    get :: NullPattern -> BnfcIdent
    get name = BnfcIdent (name ^. 
        coercedTo @((Int,Int), String) 
        % swapped)
