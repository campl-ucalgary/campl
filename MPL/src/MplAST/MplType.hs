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
module MplAST.MplType where


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

type family XMplType x
type family TypeP x

type family XTypeWithArgs x
type family XTypeVar x
type family XXType x

type family XTypeIntF x
type family XTypeCharF x
type family XTypeDoubleF x

type family XTypeStringF x
type family XTypeUnitF x
type family XTypeBoolF x
type family XTypeListF x
type family XTypeTupleF x

type family XTypeSeqWithArgs x
-- type family XTypeSeqVar x

type family XTypeGet x
type family XTypePut x
type family XTypeTensor x
type family XTypePar x
type family XTypeTopBot x
type family XTypeNeg x

type family XTypeSeqArrF x
type family XTypeConcArrF x


data MplType x  =
    TypeVar !(XTypeVar x) (TypeP x) 
    | TypeSeqWithArgs !(XTypeSeqWithArgs x) (IdP x) [MplType x]
    | TypeConcWithArgs !(XTypeSeqWithArgs x) (IdP x) ([MplType x], [MplType x])

    | TypeSeq !(MplSeqTypesF x (MplType x))
    | TypeConc !(MplConcTypesF x (MplType x))
    | TypeArr !(MplArrTypesF x (MplType x))

    | XType !(XXType x)


-- pattern UTypeWithArgs id args = TypeWithArgs () id args
-- pattern UTypeVar id args = TypeVar () id args
--
data MplBuiltInTypes x r 

data MplSeqTypesF x r =
    -- primitive types
    TypeIntF !(XTypeIntF x)
    | TypeCharF !(XTypeCharF x)
    | TypeDoubleF !(XTypeDoubleF x)

    -- built in types
    | TypeStringF !(XTypeStringF x)
    | TypeUnitF !(XTypeUnitF x)
    | TypeBoolF !(XTypeBoolF x)
    | TypeListF !(XTypeListF x) r
    | TypeTupleF !(XTypeTupleF x) (r, r, [r])

    -- | TypeSeqVar !(XTypeVar x) (TypeP x) [r]
  deriving (Functor, Foldable, Traversable)

data MplConcTypesF x r = 
    TypeGetF !(XTypeGet x) r r
    | TypePutF !(XTypePut x) r r
    | TypeTensorF !(XTypeTensor x) r r
    | TypeParF !(XTypePar x) r r
    | TypeNegF !(XTypeNeg x) r
    | TypeTopBotF !(XTypeTopBot x)
  deriving (Functor, Foldable, Traversable)


data MplArrTypesF x t =
    TypeSeqArrF !(XTypeSeqArrF x) (NonEmpty t) t
    | TypeConcArrF !(XTypeConcArrF x) [t] [t] [t]
  deriving (Functor, Foldable, Traversable)

type ForallMplType (c :: Type -> Constraint) x =
    ( c (XMplType x)
    , c (TypeP x)
    , c (XTypeWithArgs x)
    , c (XTypeVar x)
    , c (XXType x)

    , c (MplSeqTypesF x (MplType x))
    , c (XTypeIntF x)
    , c (XTypeCharF x)
    , c (XTypeDoubleF x)
    , c (XTypeStringF x)
    , c (XTypeUnitF x)
    , c (XTypeBoolF x)
    , c (XTypeListF x)
    , c (XTypeTupleF x)
    , c (XTypeSeqWithArgs x)
    -- , c (XTypeSeqVar x)

    , c (MplConcTypesF x (MplType x))
    , c (XTypeGet x)
    , c (XTypePut x)
    , c (XTypeTensor x)
    , c (XTypePar x)
    , c (XTypeTopBot x)
    , c (XTypeNeg x)

    , c (XTypeSeqArrF x)
    , c (XTypeConcArrF x)
    )

deriving instance 
    ( ForallMplType Show x
    , Show (IdP x) ) => 
    Show (MplType x)

deriving instance 
    ( ForallMplType Show x
    , Show (IdP x) ) => 
    Show (MplSeqTypesF x (MplType x))

deriving instance 
    ( ForallMplType Show x
    , Show (IdP x) ) => 
    Show (MplConcTypesF x (MplType x))

deriving instance 
    ( ForallMplType Show x
    , Show (IdP x) ) => 
    Show (MplArrTypesF x (MplType x))

$(makeBaseFunctor ''MplType)
$(concat <$> traverse makeClassyPrisms
    [ ''MplType
    , ''MplSeqTypesF
    , ''MplConcTypesF
    , ''MplArrTypesF
    ]
 )
instance AsMplSeqTypesF (MplType x) x (MplType x) where
    _MplSeqTypesF = _TypeSeq

instance AsMplConcTypesF (MplType x) x (MplType x) where
    _MplConcTypesF = _TypeConc


data InternalConcTypes =
    InternalGet 
    | InternalPut
    | InternalTopBot
    | InternalTensor
    | InternalPar
  deriving ( Eq, Ord, Read, Show, Generic, Data, Typeable )

_InternalConcTypeParser :: Prism' String InternalConcTypes
_InternalConcTypeParser = prism' embed match
  where
    tmp = 
        [ ("Get", InternalGet)
        , ("Put", InternalPut)
        , ("TopBot", InternalTopBot)
        , ("(*)", InternalTensor)
        , ("(+)", InternalPar)
        ]

    embed n = fst . fromJust $ find ((n==) . snd) tmp

    match n = snd <$> find ((n==) . fst) tmp
    

data InternalSeqPrimitiveTypes =
    InternalInt
    | InternalChar 
    | InternalDouble 
  deriving ( Eq, Ord, Read, Show, Generic, Data, Typeable )

_InternalSeqPrimitiveTypeParser :: Prism' String InternalSeqPrimitiveTypes 
_InternalSeqPrimitiveTypeParser = prism' embed match
  where
    tmp = 
        [ ("Int", InternalInt)
        , ("Char", InternalChar)
        , ("Double", InternalDouble)
        ]

    embed n = fst . fromJust $ find ((n==) . snd) tmp

    match n = snd <$> find ((n==) . fst) tmp

data InternalSeqBuiltInTypes =
    InternalString 
    | InternalUnit 
    | InternalBool
    | InternalList 
  deriving (Show, Eq)

_InternalSeqBuiltInTypeParser :: Prism' String InternalSeqBuiltInTypes 
_InternalSeqBuiltInTypeParser = prism' embed match
  where
    tmp = 
        [ ("String", InternalString)
        , ("()", InternalUnit)
        , ("Bool", InternalBool)
        , ("[]", InternalList)
        ]

    embed n = fst . fromJust $ find ((n==) . snd) tmp

    match n = snd <$> find ((n==) . fst) tmp

    

{-
data SeqTypeF ident t = 
    TypeTupleF { _seqTypeArgs :: (t, NonEmpty t) }

    -- TODO -- IMPLEMENT INT INPUT ON THE FRONT END
    | TypeIntF { _seqTypeIdent :: ident }
    | TypeCharF { _seqTypeIdent :: ident }
    | TypeDoubleF { _seqTypeIdent :: ident }
    | TypeStringF { _seqTypeIdent :: ident }
    | TypeUnitF { _seqTypeIdent :: ident }
    | TypeBoolF 
    | TypeListF { _seqTypeArg :: t }
    -- arrow types (these cannot be generated from the parser but are used internally in unification)
    | TypeSeqArrF { 
        _seqArrFrom :: [t]
        , _seqArrTo :: t }

  deriving ( Read, Show, Generic, Data, Functor, Typeable, Foldable, Traversable, Eq )

data ConcTypeF ident t = 
    TypeGetF { _concTypeIdent :: ident , _concTypeSeqArg :: t,  _concTypeConcArg :: t }
    | TypePutF { _concTypeIdent :: ident , _concTypeSeqArg :: t,  _concTypeConcArg :: t }

    | TypeTensorF { _concTypeIdent :: ident , _concTypeLeftArg :: t,  _concTypeRightArg :: t }
    | TypeParF { _concTypeIdent :: ident , _concTypeLeftArg :: t,  _concTypeRightArg :: t }

    | TypeTopBotF { _concTypeIdent :: ident }

    | TypeNegF { _concTypeIdent :: ident , _concTypeArg :: t  }
    | TypeConcArrF { _concSeqArgs :: [t], _concInputArgs :: [t], _concOutputArgs :: [t] }
  deriving ( Read, Show, Generic, Data, Functor, Typeable, Foldable, Traversable, Eq )


typeCallDefTraversal :: Traversal (MplType a ident typevar) (MplType a' ident typevar) a a'
typeCallDefTraversal = traversalVL trv 
  where
    trv f (TypeWithArgs ident calldef rst) = 
        TypeWithArgs ident <$> f calldef <*> traverse (trv f) rst
    trv f (TypeVar n ns) = TypeVar n <$> traverse (trv f) ns
    trv f (TypeSeq seq) = TypeSeq <$> traverse (trv f) seq
    trv f (TypeConc conc) = TypeConc <$> traverse (trv f) conc

data InternalSeqType =
    InternalInt
    | InternalChar 
    | InternalString 
    | InternalDouble 
    | InternalUnit 
  deriving ( Eq, Ord, Read, Show, Generic, Data, Typeable )

_InternalSeqTypeParser :: Prism' String InternalSeqType
_InternalSeqTypeParser = prism' embed match
  where
    tmp = 
        [ ("Int", InternalInt)
        , ("Char", InternalChar)
        , ("Double", InternalDouble)
        , ("String", InternalString)
        ]

    embed n = fst . fromJust $ find ((n==) . snd) tmp

    match n = snd <$> find ((n==) . fst) tmp

_InternalConcTypeParser :: Prism' String InternalConcTypes
_InternalConcTypeParser = prism' embed match
  where
    tmp = 
        [ ("Get", InternalGet)
        , ("Put", InternalPut)
        , ("TopBot", InternalTopBot)
        , ("(*)", InternalTensor)
        , ("(+)", InternalPar)
        ]

    embed n = fst . fromJust $ find ((n==) . snd) tmp

    match n = snd <$> find ((n==) . fst) tmp
    
data InternalConcTypes =
    InternalGet 
    | InternalPut
    | InternalTopBot
    | InternalTensor
    | InternalPar
  deriving ( Eq, Ord, Read, Show, Generic, Data, Typeable )

{-
$(concat <$> traverse (makeFieldLabelsWith (fieldLabelsRules & lensField .~ underscoreNoPrefixNamer))
    [ ''MplType
    ]
 )
 -}
$(concat <$> traverse makeLenses
    [ ''MplType
    , ''SeqTypeF
    , ''ConcTypeF
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''MplType
    , ''SeqTypeF
    , ''ConcTypeF
    , ''InternalConcTypes
    ]
 )

$(makeBaseFunctor ''MplType)

simplifyArrow :: 
    MplType calldef ident typevar ->
    MplType calldef ident typevar 
simplifyArrow (TypeSeq (TypeSeqArrF [] n)) = n 
simplifyArrow n = n 
-}
