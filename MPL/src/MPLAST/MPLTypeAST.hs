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


data Type def var =
    TypeWithArgs { _typeIdentDef :: def, _typeArgs :: [Type def var] }
    | TypeVar { _typeIdent :: var }

    | TypeSeq (SeqTypeF def (Type def var))
    | TypeConc (ConcTypeF def (Type def var))
  deriving ( Read, Show, Generic, Out, Data, Eq )

data SeqTypeF def t = 
    TypeTupleF { _seqTypeArgs :: (t, NonEmpty t) }
    | TypeListF { _seqTypeArg :: t }

    | TypeCharF { _seqTypeIdent :: def }
    | TypeDoubleF { _seqTypeIdent :: def }
    | TypeString { _seqTypeIdent :: def }
    | TypeUnitF { _seqTypeIdent :: def }

  deriving ( Read, Show, Generic, Out, Data, Functor, Typeable, Foldable, Traversable, Eq )

data ConcTypeF def t = 
    TypeGetF { _concTypeIdent :: def , _concTypeSeqArg :: t,  _concTypeConcArg :: t }
    | TypePutF { _concTypeIdent :: def , _concTypeSeqArg :: t,  _concTypeConcArg :: t }

    | TypeTensorF { _concTypeIdent :: def , _concTypeLeftArg :: t,  _concTypeRightArg :: t }
    | TypeParF { _concTypeIdent :: def , _concTypeLeftArg :: t,  _concTypeRightArg :: t }

    | TypeTopBotF { _concTypeIdent :: def }

    | TypeNegF { _concTypeIdent :: def , _concTypeArg :: t  }
  deriving ( Read, Show, Generic, Out, Data, Functor, Typeable, Foldable, Traversable, Eq )

data KindPhrase def var =
    -- data defn
    KindPhraseDataDefn {
        _kindPhraseIdent :: def
        , _kindPhraseDataArgs :: [var]

        , _kindPhraseNeighborIdents :: [def]
        , _kindPhraseChildrenIdents :: [def]

        , _kindPhraseStateVar :: var
    }
    -- codata defn
    | KindPhraseCodataDefn {
        _kindPhraseIdent :: def
        , _kindPhraseCodataArgs :: [var]
        , _kindPhraseStateVar :: var

        , _kindPhraseNeighborIdents :: [def]
        , _kindPhraseChildrenIdents :: [def]

    }
  deriving ( Read, Show, Generic, Out, Data, Eq )

data TypePhrase def var =
    TypePhraseProcessDefn { _typePhraseIdent :: def
        , _typeProcessSeqArgs :: [Type def var]
        , _typeProcessInChs :: [Type def var]
        , _typeProcessOutChs :: [Type def var]
    }
    | TypePhraseFunctionDefn {
        _typePhraseIdent :: def
        , _typeFunctionInArgs :: [Type def var]
        , _typeFunctionOut :: Type def var
    }
    | TypePhraseDestrDefn {
        _typePhraseIdent :: def
        , _typePhraseParentIdent :: def

        , _typePhraseDestrInp :: NonEmpty (Type def var)
        , _typePhraseStateVar :: var
    } 
    | TypePhraseConstrDefn {
        _typePhraseIdent :: def
        , _typePhraseParentIdent :: def

        , _typePhraseConstrInp :: [Type def var]
        , _typePhraseStateVar :: var
    }
  deriving ( Read, Show, Generic, Out, Data, Eq )


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

$(concat <$> traverse (makeFieldLabelsWith (fieldLabelsRules & lensField .~ underscoreNoPrefixNamer))
    [ ''Type
    , ''TypePhrase 
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''Type
    , ''TypePhrase
    , ''SeqTypeF
    , ''ConcTypeF
    ]
 )

$(makeBaseFunctor ''Type)
