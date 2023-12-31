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
{-# LANGUAGE FlexibleContexts #-}
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

{- Module for defining the kinds for types (types of types)
 - Note that most of this module is unused -- we opted to go
 - with a very trivial kind checking system..
 -}

type family XMplKind x

type family XSeqKind x
type family XConcKind x
type family XArrKind x
type family XSeqArgKind x
type family XConcArgKind x
type family XKindVar x

type family KindP x

type family XXKind x

data MplPrimitiveKind x 
    = SeqKind !(XSeqKind x)
    | ConcKind !(XConcKind x)

data MplKind x 
    = PrimitiveKind !(MplPrimitiveKind x)
    | SeqArgKind !(XSeqArgKind x) [MplKind x]
    | ConcArgKind !(XConcArgKind x) ([MplKind x], [MplKind x])
    | ArrKind !(XArrKind x) (MplKind x) (MplKind x)
    | KindVar !(XKindVar x) (KindP x)
    | XKind !(XXKind x)

$(concat <$> traverse makeClassyPrisms 
    [ ''MplPrimitiveKind
    , ''MplKind ]
 )
$(makeBaseFunctor ''MplKind)

deriving instance (Show (XSeqKind x), Show (XConcKind x)) => 
    Show (MplPrimitiveKind x)

deriving instance (Eq (XSeqKind x), Eq (XConcKind x)) => 
    Eq (MplPrimitiveKind x)

instance AsMplPrimitiveKind (MplKind x) x where
    _MplPrimitiveKind = _PrimitiveKind

type ForallMplKind (c :: Type -> Constraint) x =
    ( c (XMplKind x)

    , c (XArrKind x)
    , c (XSeqKind x)
    , c (XConcKind x)
    , c (XSeqArgKind x)
    , c (XConcArgKind x)

    , c (XKindVar x)
    , c (KindP x)

    , c (XXKind x)
    )
