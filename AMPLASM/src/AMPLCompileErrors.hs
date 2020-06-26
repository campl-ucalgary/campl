{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
module AMPLCompileErrors where

import Language.ParAMPL
import Language.LexAMPL
import Language.AbsAMPL
import Language.ErrM
import Language.LayoutAMPL

import AMPLSymbolTable
import AMPLConstructBag
import AMPLTypes
import AMPLAST

class ErrorToStr e where
    errorToStr :: e -> String

class HasFreeVarError e where
    freeVar :: Ident -> e

newtype FreeVarError =
    FreeVarError Ident
  deriving Show

instance HasFreeVarError FreeVarError where
    freeVar = FreeVarError

class HasFreeChannelError e where
    freeChannel :: Ident -> e

newtype FreeChannelError =
    FreeChannelError Ident
  deriving Show

instance HasFreeChannelError FreeChannelError where
    freeChannel = FreeChannelError

class HasIllegalInstrCallError e e' | e -> e' where
    illegalInstrCall :: Ident -> e' ->  e

data IllegalInstrCall e' =
    IllegalInstrCall Ident e'
  deriving Show

instance HasIllegalInstrCallError (IllegalInstrCall e) e where
    illegalInstrCall = IllegalInstrCall

class HasFunctionArityMismatchError e where
    functionArityMismatch :: (Ident, [Ident]) -> (RowColPos, [Ident]) -> e
        -- ^ (function name, expected args), ( called position, given args)
data FunctionArityMismatch = FunctionArityMismatch (Ident, [Ident]) (RowColPos, [Ident])
  deriving Show

instance HasFunctionArityMismatchError FunctionArityMismatch where
    functionArityMismatch = FunctionArityMismatch

class HasDataArityMismatchError e where
    dataArityMismatch :: (Ident, Ident, Word) -> (RowColPos, RowColPos, [Ident]) -> e
        -- ^ (datatype, constructor, expected args), (called positions, args) )

data DataArityMismatch = DataArityMismatch (Ident, Ident, Word) (RowColPos, RowColPos, [Ident])
  deriving Show

instance HasDataArityMismatchError DataArityMismatch where
    dataArityMismatch = DataArityMismatch

class HasCodataArityMismatchError e where
    codataArityMismatch :: (Ident, Ident, Word) -> (RowColPos, RowColPos, [Ident]) -> e
        -- ^ (datatype, constructor, expected args), (called positions, args) )

data CodataArityMismatch = CodataArityMismatch (Ident, Ident, Word) (RowColPos, RowColPos, [Ident])
  deriving Show

instance HasCodataArityMismatchError CodataArityMismatch where
    codataArityMismatch = CodataArityMismatch

class HasCasingOverMultipleDatas e where
    casingOverMultipleDatas :: [Ident] -> e

newtype CasingOverMultipleDatasError = CasingOverMultipleDatasError [Ident]
  deriving Show

instance HasCasingOverMultipleDatas CasingOverMultipleDatasError where
    casingOverMultipleDatas = CasingOverMultipleDatasError

class HasHCasingOverMultipleTypes e where
    hcasingOverMultipleTypes :: [Ident] -> e

newtype HCasingOverMultipleDatasError = HCasingOverMultipleDatasError [Ident]
  deriving Show

instance HasHCasingOverMultipleTypes HCasingOverMultipleDatasError where
    hcasingOverMultipleTypes = HCasingOverMultipleDatasError

class HasRecordOverMultipleCodatas e where
    recordOverMultipleCodatas :: [Ident] -> e

newtype RecordOverMultipleCodatasError = RecordOverMultipleCodatasError [Ident]
  deriving Show

instance HasRecordOverMultipleCodatas RecordOverMultipleCodatasError where
    recordOverMultipleCodatas = RecordOverMultipleCodatasError

data ProcessArityMismatch =
    ProcessArityMismatch  (Ident, [Ident], [Ident], [Ident]) (RowColPos, [Ident], [Ident], [Ident]) 
  deriving Show

class HasProcessArityMismatch e where
    processArityMismatch :: (Ident, [Ident], [Ident], [Ident]) -> (RowColPos, [Ident], [Ident], [Ident]) -> e

instance HasProcessArityMismatch ProcessArityMismatch where
    processArityMismatch = ProcessArityMismatch

class HasPolarityMismatch e where
    polarityMismatch :: Polarity -> (Ident, Polarity) -> e

data PolarityMismatch = PolarityMismatch Polarity (Ident, Polarity) 
  deriving Show

instance HasPolarityMismatch PolarityMismatch where
    polarityMismatch = PolarityMismatch

class HasHCaseArityMismatch e where
    hcaseArityMismatch :: (Ident, Ident, Word) -> (RowColPos, RowColPos, [Ident]) -> e

data HCaseArityMismatch = HCaseArityMismatch (Ident, Ident, Word) (RowColPos, RowColPos, [Ident])
  deriving Show

instance HasHCaseArityMismatch HCaseArityMismatch where
    hcaseArityMismatch = HCaseArityMismatch
    
type CompilerErrors e = 
    ( HasAmbiguousLookupError e 
    , HasHCaseArityMismatch e
    , HasIllegalInstrCallError e e
    , HasFunctionArityMismatchError e
    , HasCasingOverMultipleDatas e
    , HasRecordOverMultipleCodatas e
    , HasHCasingOverMultipleTypes e
    , HasDataArityMismatchError e
    , HasCodataArityMismatchError e
    , HasProcessArityMismatch e
    , HasPolarityMismatch e
    , HasFreeVarError e
    , HasFreeChannelError e
    , HasCodataLookupError e
    , HasCoprotocolLookupError e
    , HasDataLookupError e
    , HasProtocolLookupError  e
    , HasLookupProcessError e
    , HasLookupFunctionError e)

