{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module MPLAST.MPLASTTranslate where

import Optics.TH
import Optics.Prism
import Optics.Operators
import Optics.Getter

import Data.Function

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE 

import MPLAST.MPLTypeAST
import MPLAST.MPLPatternAST
import MPLAST.MPLExprAST
import MPLAST.MPLProcessCommandsAST
import MPLAST.MPLProg

import qualified Language.AbsMPL as B

import GHC.Generics
import Data.Data
import Data.Typeable

import Text.PrettyPrint.GenericPretty


translateBnfcMplToProg :: B.MplProg -> Prog (String, Maybe (Int, Int))
translateBnfcMplToProg = undefined

