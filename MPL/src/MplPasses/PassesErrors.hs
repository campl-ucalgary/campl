{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.PassesErrors where

import Optics
import Optics.TH

import Data.List

import qualified MplPasses.Parser.BnfcParse as B
import qualified MplPasses.Parser.Parse as P
import qualified MplPasses.Parser.ParseErrors as P
import qualified MplPasses.Renamer.Rename as R
import qualified MplPasses.Renamer.RenameErrors as R

import MplPasses.TypeChecker.TypeCheck
import qualified MplPasses.TypeChecker.TypeCheckErrors as T
import MplPasses.TypeChecker.KindCheck 
import MplPasses.TypeChecker.TypeEqns 
import MplPasses.TypeChecker.TypeCheckSemanticErrors 
import MplPasses.TypeChecker.TypeCheckCallErrors 
import MplPasses.TypeChecker.TypeCheckErrorPkg 
import MplPasses.TypeChecker.TypeCheckMplTypeSub

import qualified MplPasses.PatternCompiler.PatternCompileErrors as PC

import MplPasses.PassesErrorsPprint

data MplPassesErrors =
    MplBnfcErrors B.BnfcErrors
    | MplParseErrors P.ParseErrors
    | MplRenameErrors R.RenameErrors
    | MplTypeCheckErrors T.TypeCheckErrors
    | MplPatternCompilationErrors PC.PatternCompileErrors 
  deriving Show

$(makeClassyPrisms ''MplPassesErrors)

instance B.AsBnfcErrors MplPassesErrors where
    _BnfcErrors = _MplBnfcErrors

instance P.AsParseErrors MplPassesErrors where
    _ParseErrors = _MplParseErrors

instance R.AsRenameErrors MplPassesErrors where 
    _RenameErrors = _MplRenameErrors

instance T.AsTypeCheckErrors MplPassesErrors where
    _TypeCheckErrors = _MplTypeCheckErrors 

instance AsTypeCheckSemanticErrors MplPassesErrors where
    _TypeCheckSemanticErrors = _MplTypeCheckErrors % _TypeCheckSemanticErrors 

instance AsKindCheckErrors MplPassesErrors where
    _KindCheckErrors = _MplTypeCheckErrors % _TypeCheckKindErrors 

instance AsTypeCheckCallErrors MplPassesErrors where
    _TypeCheckCallErrors = _MplTypeCheckErrors % _TypeCheckCallErrors 

instance AsTypeUnificationError MplPassesErrors MplTypeSub where
    _TypeUnificationError = _MplTypeCheckErrors % _TypeUnificationError 

instance PC.AsPatternCompileErrors MplPassesErrors where
    _PatternCompileErrors  = _MplPatternCompilationErrors 


pprintMplPassesErrors :: MplPassesErrors -> MplDoc
pprintMplPassesErrors = vsep . go
  where
    go :: MplPassesErrors -> [MplDoc]
    go (MplBnfcErrors (B.BnfcParseError err)) = 
        [ pretty "Error during BNFC parsing:"
        , indent' $ pretty err
        ] 
    go (MplParseErrors err) = 
        [ pretty "Error during parsing:"
        , indent' $ P.pprintParseErrors err
        ]
    go (MplRenameErrors err) = 
        [ pretty "Error during renaming:"
        , indent' $ R.pprintRenameErrors err
        ]
    go (MplTypeCheckErrors err) = 
        [ pretty "Error during type checking:"
        , indent' $ T.pprintTypeCheckErrors err
        ]
    go (MplPatternCompilationErrors err) = 
        [ pretty "Error during pattern compilation:"
        , indent' $ PC.pprintPatternCompileErrors err
        ]

    indent' = indent 4
