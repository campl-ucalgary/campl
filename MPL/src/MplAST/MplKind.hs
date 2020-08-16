{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
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
module MplAST.MplKind where


import Optics
import Data.Functor.Foldable.TH
import MplAST.MplIdent
import MplAST.MplExt

import Data.Function
import Data.List.NonEmpty (NonEmpty (..))
import Data.List
import Data.Maybe
import Data.Kind
import qualified Data.List.NonEmpty as NE 

import GHC.Generics
import Data.Data
import Data.Typeable

import Text.PrettyPrint.GenericPretty

type family XMplKind x

type family XSeqKind x
type family XConcKind x
type family XArrKind x

type family XXKind x

data MplPrimitiveKind x = 
    SeqKind !(XSeqKind x)
    | ConcKind !(XConcKind x)

data MplKind x =
    PrimitiveKind !(MplPrimitiveKind x)
    | ArrKind !(XArrKind x) (MplKind x) (MplKind x)
    | XKind !(XXKind x)

$(concat <$> traverse makeClassyPrisms 
    [ ''MplPrimitiveKind
    , ''MplKind ]
 )
$(makeBaseFunctor ''MplKind)

instance AsMplPrimitiveKind (MplKind x) x where
    _MplPrimitiveKind = _PrimitiveKind

type ForallMplKind (c :: Type -> Constraint) x =
    ( c (XMplKind x)

    , c (XSeqKind x)
    , c (XConcKind x)
    , c (XArrKind x)

    , c (XXKind x)
    )
