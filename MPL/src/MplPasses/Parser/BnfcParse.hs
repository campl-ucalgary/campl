{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module MplPasses.Parser.BnfcParse (
    module Language.AbsMPL
    , module Language.ErrM
    , module Language.LayoutMPL
    , module Language.LexMPL
    , module Language.ParMPL
    , module Language.PrintMPL
    , module Language.SkelMPL
    , runBnfc
    , BnfcErrors (..)
    , AsBnfcErrors (..)
    ) where

import Optics

import Language.AbsMPL
import Language.ErrM
import Language.LayoutMPL
import Language.LexMPL
import Language.ParMPL
import Language.PrintMPL
import Language.SkelMPL

{- Module for defining wrappers around BNFC's generated code
 -}

newtype BnfcErrors = 
    BnfcParseError String
  deriving Show

$(makeClassyPrisms ''BnfcErrors)


runBnfc :: AsBnfcErrors err => 
    String -> 
    Either [err] MplProg
runBnfc = 
    \case 
        Ok a -> Right a
        Bad str -> Left [_BnfcParseError # str]
    . pMplProg 
    . resolveLayout True 
    . myLexer

