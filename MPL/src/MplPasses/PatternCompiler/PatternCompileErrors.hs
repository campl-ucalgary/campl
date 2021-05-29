{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module MplPasses.PatternCompiler.PatternCompileErrors where

import Optics

import Data.Coerce

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import MplAST.MplPatternCompiled

import MplPasses.PassesErrorsPprint

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

pprintPatternCompileErrors :: PatternCompileErrors -> MplDoc
pprintPatternCompileErrors = go
  where
    go :: PatternCompileErrors -> MplDoc
    go = \case
        NonExhaustiveFunPatt identr -> hsep
            [ pretty "Non-exhaustive pattern with function"
            , pprintIdentPWithLoc (identr ^. identRIdentP)
            ]
        NonExhaustiveProcPatt identr -> hsep
            [ pretty "Non-exhaustive pattern with process"
            , pprintIdentPWithLoc (identr ^. identRIdentP)
            ]
        NonExhaustiveECasePatt  -> hsep
            [ pretty "Non-exhaustive expression case" ]

        NonExhaustiveSwitch  -> hsep
            [ pretty "Non-exhaustive expression switch (there must be a `True' to be exhaustive)" ]

        NonExhaustiveRecordPatt identr->  hsep
            [ pretty "Non-exhaustive record pattern with"
            , pprintIdentPWithLoc (identr ^. identRIdentP)
            ]
        
        NonExhaustiveGet keynameocc ->  hsep
            [ pretty "Non-exhaustive `get' command pattern at"
            , pprintLoc $ (coerce keynameocc :: NameOcc) ^. nameOccLocation
            ]
        NonExhaustiveCCasePatt ->  hsep
            [ pretty  "Non-exhaustive command case." ]

        NonExhaustiveCSwitch -> hsep
            [ pretty "Non-exhaustive command switch (there must be a `True' to be exhaustive)." ]
