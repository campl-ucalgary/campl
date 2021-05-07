{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module MplPasses.TypeInferencer.TypeInference where

import Optics 
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked 

import MplPasses.Env

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Control.Arrow
import Control.Applicative
import Data.Semigroup
import Data.Maybe
import Data.Bool
import Data.Function
import Data.Traversable
import Data.Coerce

import qualified Data.Map as Map
import Data.Map (Map)

import Data.List
import Debug.Trace

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Data.Foldable

import Data.Functor.Foldable (Base, cata, para)
import Data.Tuple

import MplUtil.UniqueSupply

import Data.Foldable

import Data.Functor.Foldable (Base, cata, para)
import Data.Tuple

import MplPasses.TypeInferencer.TypeInferenceError
