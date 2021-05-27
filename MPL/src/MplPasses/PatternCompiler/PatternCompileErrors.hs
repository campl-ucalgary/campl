{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module MplPasses.PatternCompiler.PatternCompileErrors where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import MplAST.MplPatternCompiled

data PatternCompileErrors 
    = NonExhaustiveFunPatt IdentT 
    | NonExhaustiveProcPatt IdentT 

    | NonExhaustiveECasePatt  
    | NonExhaustiveSwitch  

    | NonExhaustiveRecordPatt IdentT

    | NonExhaustiveGet KeyWordNameOcc
    | NonExhaustiveCCasePatt  

    | NonExhaustiveCSwitch 

  deriving Show



$(makeClassyPrisms ''PatternCompileErrors)
