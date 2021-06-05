{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module MplPasses.Parser.BnfcParse (
    module MplLanguage.AbsMPL
    , module MplLanguage.ErrM
    , module MplLanguage.LayoutMPL
    , module MplLanguage.LexMPL
    , module MplLanguage.ParMPL
    , module MplLanguage.PrintMPL
    , runBnfc
    , BnfcErrors (..)
    , AsBnfcErrors (..)
    ) where

import Optics

import MplLanguage.AbsMPL
import MplLanguage.ErrM
import MplLanguage.LayoutMPL
import MplLanguage.LexMPL
import MplLanguage.ParMPL
import MplLanguage.PrintMPL

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

