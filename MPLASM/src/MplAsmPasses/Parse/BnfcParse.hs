{-# LANGUAGE LambdaCase #-}
module MplAsmPasses.Parse.BnfcParse where

import Optics

import MplAsmPasses.Parse.ParseErrors

import MplAsmLanguage.ParMPLASM
import MplAsmLanguage.AbsMPLASM
import MplAsmLanguage.LexMPLASM 
import MplAsmLanguage.SkelMPLASM
import MplAsmLanguage.ErrM
import MplAsmLanguage.LayoutMPLASM

bnfcParse :: 
    AsParseError err =>
    String ->
    Either [err] AMPLCODE 
bnfcParse = 
    \case 
        Ok a -> Right a
        Bad str -> Left [_BnfcParseError # str]
    . pAMPLCODE 
    . resolveLayout True 
    . myLexer
