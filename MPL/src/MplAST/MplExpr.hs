{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RoleAnnotations #-}
module MplAST.MplExpr where


import Optics.TH
import Optics.Prism
import Optics.Operators
import Data.Functor.Foldable.TH

import MplAST.MplIdent

import Data.Function
import Data.List.NonEmpty (NonEmpty (..))
import Data.List
import Data.Maybe
import qualified Data.List.NonEmpty as NE 

import MplAST.MplPattern

import GHC.Generics
import Data.Data
import Data.Typeable
import Data.Kind

import Data.Void

{- Module for things relating to a sequential expression...
 - Note that this only contains built in primitives / built in operators
 - because an MPL expression can be recursive with an MPL statement in a 
 - let binding... So the definition of an MplExpr is in MplProg.hs
 -}

type family XMplExpr x

_PrimitiveOperatorParser :: Prism' String PrimitiveOperators
_PrimitiveOperatorParser = prism' embed match
  where
    tmp = 
        [ ("==", PrimitiveEq)
        , ("==", PrimitiveNeq)
        , ("<", PrimitiveLt)
        , ("<=", PrimitiveLeq)
        , (">", PrimitiveGt)
        , (">=", PrimitiveGeq)
        , ("+", PrimitiveAdd)
        , ("-", PrimitiveAdd)
        , ("*", PrimitiveMul)
        , ("/", PrimitiveDiv)
        , ("%", PrimitiveMod)
        , ("^", PrimitiveExp)
        ]

    embed n = fst . fromJust $ find ((n==) . snd) tmp

    match n = snd <$> find ((n==) . fst) tmp

    
data PrimitiveOperators =
    PrimitiveAdd
    | PrimitiveSub
    | PrimitiveMul
    | PrimitiveDiv
    | PrimitiveMod
    | PrimitiveExp
    | PrimitiveEq
    | PrimitiveNeq
    | PrimitiveLt
    | PrimitiveLeq
    | PrimitiveGt
    | PrimitiveGeq
  deriving (Show, Read, Eq, Data, Generic)

_BuiltInOperatorParser :: Prism' String BuiltInOperators
_BuiltInOperatorParser = prism' embed match
  where
    tmp = 
        [ ("||", InternalOr)
        , ("&&", InternalAnd)
        , ("++", InternalAppend)
        , ("!!", InternalIndex)
        , (":", InternalListCons)
        ]

    embed n = fst . fromJust $ find ((n==) . snd) tmp

    match n = snd <$> find ((n==) . fst) tmp

data BuiltInOperators = 
    InternalOr
    | InternalAnd
    | InternalAppend
    | InternalIndex
    | InternalListCons
  deriving (Show, Read, Eq, Data, Generic)
