{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
module MPLAST.MPLTypeAST where

import MPLUtil.Data.List.NonEmpty

import Optics
import Data.Functor.Foldable.TH

import Data.Function
import Data.List.NonEmpty (NonEmpty (..))
import Data.List
import Data.Maybe
import qualified Data.List.NonEmpty as NE 

import GHC.Generics
import Data.Data
import Data.Typeable

import Text.PrettyPrint.GenericPretty

-- TODO: for obvious reasons, this is not ideal. Perhaps look into
-- autmoatically genrating the required parts with Template haskell.
-- #define MplAstDerivingClause ( Read, Show, Generic, Out, Functor, Foldable, Traversable, Data, Typeable )
#define MPL_TYPE_AST_PLAIN_DATA_DERIVING_CLAUSE ( Eq, Ord, Read, Show, Generic, Out, Data, Typeable )

data Type calldef ident typevar =
    TypeWithArgs { 
        _typeIdent :: ident
        , _typeCallDef :: calldef
        , _typeArgs :: [Type calldef ident typevar] }
    | TypeVar { 
        _typeVarIdent :: typevar 
        , _typeArgs :: [Type calldef ident typevar] 
            -- normally, typeArgs is empty. If we have higher kinded 
            -- data, this is NON empty
        }

    | TypeSeq (SeqTypeF ident (Type calldef ident typevar))
    | TypeConc (ConcTypeF ident (Type calldef ident typevar))
  deriving ( Read, Show, Generic, Out, Data, Eq, Functor, Foldable, Traversable )

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

  deriving ( Read, Show, Generic, Out, Data, Functor, Typeable, Foldable, Traversable, Eq )

data ConcTypeF ident t = 
    TypeGetF { _concTypeIdent :: ident , _concTypeSeqArg :: t,  _concTypeConcArg :: t }
    | TypePutF { _concTypeIdent :: ident , _concTypeSeqArg :: t,  _concTypeConcArg :: t }

    | TypeTensorF { _concTypeIdent :: ident , _concTypeLeftArg :: t,  _concTypeRightArg :: t }
    | TypeParF { _concTypeIdent :: ident , _concTypeLeftArg :: t,  _concTypeRightArg :: t }

    | TypeTopBotF { _concTypeIdent :: ident }

    | TypeNegF { _concTypeIdent :: ident , _concTypeArg :: t  }
  deriving ( Read, Show, Generic, Out, Data, Functor, Typeable, Foldable, Traversable, Eq )

typeCallDefTraversal :: Traversal (Type a ident typevar) (Type a' ident typevar) a a'
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
  deriving MPL_TYPE_AST_PLAIN_DATA_DERIVING_CLAUSE 

_InternalSeqTypeParser :: Prism' String InternalSeqType
_InternalSeqTypeParser = prism' embed match
  where
    tmp = 
        [ ("Int", InternalInt)
        , ("Char", InternalChar)
        , ("Double", InternalDouble)
        , ("String", InternalString)
        , ("Unit", InternalUnit)
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
        , ("Tensor", InternalTensor)
        , ("Par", InternalPar)
        ]

    embed n = fst . fromJust $ find ((n==) . snd) tmp

    match n = snd <$> find ((n==) . fst) tmp
    
data InternalConcTypes =
    InternalGet 
    | InternalPut
    | InternalTopBot
    | InternalTensor
    | InternalPar
  deriving MPL_TYPE_AST_PLAIN_DATA_DERIVING_CLAUSE 

{-
$(concat <$> traverse (makeFieldLabelsWith (fieldLabelsRules & lensField .~ underscoreNoPrefixNamer))
    [ ''Type
    ]
 )
 -}
$(concat <$> traverse makeLenses
    [ ''Type
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''Type
    , ''SeqTypeF
    , ''ConcTypeF
    ]
 )

$(makeBaseFunctor ''Type)

simplifyArrow :: 
    Type calldef ident typevar ->
    Type calldef ident typevar 
simplifyArrow (TypeSeq (TypeSeqArrF [] n)) = n 
simplifyArrow n = n 
