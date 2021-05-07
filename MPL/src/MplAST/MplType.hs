{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
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

{- Module for defining the AST for MPL Types.
  -}


type family XMplType x
type family TypeP x

type family XTypeVar x
type family XTypeWithNoArgs x

type family XTypeSeqWithArgs x
type family XTypeSeqVarWithArgs x
type family XTypeConcWithArgs x
type family XTypeConcVarWithArgs  x

type family XXType x

type family XTypeIntF x
type family XTypeCharF x
type family XTypeDoubleF x

type family XTypeStringF x
type family XTypeUnitF x
type family XTypeBoolF x
type family XTypeListF x
type family XTypeTupleF x

-- type family XTypeSeqVar x

type family XTypeGet x
type family XTypePut x
type family XTypeTensor x
type family XTypePar x
type family XTypeTopBot x
type family XTypeNeg x

type family XTypeSeqArrF x
type family XTypeConcArrF x

type family XXMplBuiltInTypesF x

data MplType x  =
    TypeVar !(XTypeVar x) (TypeP x) 
    -- | Why is this here? Well, we previously could have @Bool@ which is different from @Bool()@ which
    -- is different from @Bool(| =>)@ i.e., we do not know which sequential vs concurrent immediately from the
    -- context of just writing @Bool@.
    -- Indeed, this is NOT a type variable, and the annotation is important to realize which type it is.
    | TypeWithNoArgs !(XTypeWithNoArgs x) (IdP x) 

    | TypeSeqWithArgs !(XTypeSeqWithArgs x) (IdP x) [MplType x]
    | TypeSeqVarWithArgs !(XTypeSeqVarWithArgs x) (TypeP x) [MplType x]

    | TypeConcWithArgs !(XTypeConcWithArgs x) (IdP x) ([MplType x], [MplType x])
    | TypeConcVarWithArgs !(XTypeConcVarWithArgs x) (TypeP x) ([MplType x], [MplType x])

    | TypeBuiltIn !(MplBuiltInTypesF x (MplType x))

    | XType !(XXType x)

data MplBuiltInTypesF x r =
    -- primitive sequential types
    TypeIntF !(XTypeIntF x)
    | TypeCharF !(XTypeCharF x)
    | TypeDoubleF !(XTypeDoubleF x)
    -- primitive concurrent types
    | TypeGetF !(XTypeGet x) r r
    | TypePutF !(XTypePut x) r r
    | TypeTensorF !(XTypeTensor x) r r
    | TypeParF !(XTypePar x) r r
    | TypeNegF !(XTypeNeg x) r
    | TypeTopBotF !(XTypeTopBot x)

    -- built in non primitive types
    | TypeStringF !(XTypeStringF x)
    | TypeUnitF !(XTypeUnitF x)
    | TypeBoolF !(XTypeBoolF x)
    | TypeListF !(XTypeListF x) r
    | TypeTupleF !(XTypeTupleF x) (r, r, [r])

    -- arrow types
    | TypeSeqArrF !(XTypeSeqArrF x) (NonEmpty r) r
    | TypeConcArrF !(XTypeConcArrF x) [r] [r] [r]

    | XTypeBuiltIn !(XXMplBuiltInTypesF x)
  deriving (Functor, Foldable, Traversable)

embedBuiltInTypes :: 
    ( XTypeTupleF x1 ~ XTypeTupleF x2
    , XTypeStringF x1 ~ XTypeStringF x2
    , XTypeTensor x1 ~ XTypeTensor x2
    , XTypeCharF x1 ~ XTypeCharF x2
    , XTypeDoubleF x1 ~ XTypeDoubleF x2
    , XTypePar x1 ~ XTypePar x2
    , XTypeUnitF x1 ~ XTypeUnitF x2
    , XTypeSeqArrF x1 ~ XTypeSeqArrF x2
    , XTypeListF x1 ~ XTypeListF x2
    , XTypeTopBot x1 ~ XTypeTopBot x2
    , XTypePut x1 ~ XTypePut x2
    , XTypeIntF x1 ~ XTypeIntF x2
    , XTypeGet x1 ~ XTypeGet x2
    , XTypeNeg x1 ~ XTypeNeg x2
    , XTypeBoolF x1 ~ XTypeBoolF x2
    , XTypeConcArrF x1 ~ XTypeConcArrF x2 ) =>
    MplBuiltInTypesF x1 r -> 
    MplBuiltInTypesF x2 r
embedBuiltInTypes (TypeIntF cxt) = TypeIntF cxt
embedBuiltInTypes (TypeCharF cxt) = TypeCharF cxt
embedBuiltInTypes (TypeDoubleF cxt) = TypeDoubleF cxt
embedBuiltInTypes (TypeGetF cxt a b) = TypeGetF cxt a b
embedBuiltInTypes (TypePutF cxt a b) = TypePutF cxt a b
embedBuiltInTypes (TypeTensorF cxt a b) = TypeTensorF cxt a b
embedBuiltInTypes (TypeParF cxt a b) = TypeParF cxt a b
embedBuiltInTypes (TypeNegF cxt a) = TypeNegF cxt a
embedBuiltInTypes (TypeTopBotF cxt) = TypeTopBotF cxt
embedBuiltInTypes (TypeStringF cxt) = TypeStringF cxt
embedBuiltInTypes (TypeUnitF cxt) = TypeUnitF cxt
embedBuiltInTypes (TypeBoolF cxt) = TypeBoolF cxt
embedBuiltInTypes (TypeListF cxt a) = TypeListF cxt a
embedBuiltInTypes (TypeTupleF cxt (a, b, c)) = TypeTupleF cxt (a, b, c)
embedBuiltInTypes (TypeSeqArrF cxt args to) = TypeSeqArrF cxt args to
embedBuiltInTypes (TypeConcArrF cxt seqs ins outs) = TypeConcArrF cxt seqs ins outs


type ForallMplType (c :: Type -> Constraint) x =
    ( c (XMplType x)
    , c (TypeP x)
    , c (XTypeVar x)
    , c (XTypeWithNoArgs x)

    , c (XTypeSeqWithArgs x)
    , c (XTypeSeqVarWithArgs x)
    , c (XTypeConcWithArgs x)
    , c (XTypeConcVarWithArgs  x)

    , c (XXType x)

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

    , c (XTypeGet x)
    , c (XTypePut x)
    , c (XTypeTensor x)
    , c (XTypePar x)
    , c (XTypeTopBot x)
    , c (XTypeNeg x)

    , c (XTypeSeqArrF x)
    , c (XTypeConcArrF x)
    
    , c (MplBuiltInTypesF x (MplType x))

    , c (XXMplBuiltInTypesF x)
    )

deriving instance 
    ( ForallMplType Show x
    , Show (IdP x) ) => 
    Show (MplType x)

deriving instance 
    ( ForallMplType Show x
    , Show (IdP x) ) => 
    Show (MplBuiltInTypesF x (MplType x))


$(makeBaseFunctor ''MplType)
$(concat <$> traverse makeClassyPrisms
    [ ''MplType
    , ''MplBuiltInTypesF
    ]
 )
instance AsMplBuiltInTypesF (MplType x) x (MplType x) where
    _MplBuiltInTypesF = _TypeBuiltIn


data InternalConcTypes =
    InternalGet 
    | InternalPut
    | InternalTopBot
    | InternalTensor
    | InternalPar
    | InternalNeg
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
        , ("Neg", InternalNeg)
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
