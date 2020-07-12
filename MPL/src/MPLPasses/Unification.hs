{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module MPLPasses.Unification where

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST

import Control.Monad

import Optics
import Data.Bool
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Control.Monad.State

data TypeEqns ident typevar =
    TypeEqnsEq (Type () ident typevar, Type () ident typevar)
    | TypeEqnsExist [ident] [TypeEqns ident typevar]
    | TypeEqnsForall [ident] [TypeEqns ident typevar]

$(concat <$> traverse makeBaseFunctor 
    [ ''TypeEqns ]
 )
$(concat <$> traverse makePrisms 
    [ ''TypeEqns 
    ]
 )

-- substitutes subs ty = foldr substitute ty subs

{-
substitute :: (Eq typevar) =>
     (typevar, Type calldef ident typevar) -> 
     Type calldef ident typevar -> 
     Type calldef ident typevar
substitute (v, sub) = cata f
  where
    f (TypeVarF ident) 
        | v == ident = sub
        | otherwise = TypeVar ident
    f n = embed n
    -}


newtype TypeTag = TypeTag UniqueTag
  deriving (Eq, Ord)

freshTypeTag ::
    ( MonadState c m
    , HasUniqueTag c ) => 
    m TypeTag
freshTypeTag = TypeTag <$> freshUniqueTag


type TaggedType = Type () TaggedBnfcIdent TypeTag

substitute :: 
    (TaggedBnfcIdent, Type () TaggedBnfcIdent TypeTag) -> 
    TypeG TaggedBnfcIdent -> 
    Type () TaggedBnfcIdent TypeTag
substitute sub typeg = cata f typeg
  where
    f = undefined

substitutes :: 
    [(TaggedBnfcIdent, Type () TaggedBnfcIdent TypeTag)] -> 
    TypeG TaggedBnfcIdent -> 
    Type () TaggedBnfcIdent TypeTag
substitutes = undefined

-- unsafe substitutions that just blindly substitute 
forceSubstitutes :: 
    [(TaggedBnfcIdent, Type () TaggedBnfcIdent TypeTag)] -> 
    TypeG TaggedBnfcIdent -> 
    Maybe (Type () TaggedBnfcIdent TypeTag)
forceSubstitutes subs typeg = cata f $ typeg & typeCallDefTraversal .~ ()
  where
    f :: TypeF 
        () TaggedBnfcIdent TaggedBnfcIdent 
        (Maybe (Type () TaggedBnfcIdent TypeTag)) -> 
        Maybe (Type () TaggedBnfcIdent TypeTag)
    f (TypeWithArgsF a () bs) = TypeWithArgs a () <$> sequenceA bs
    f (TypeVarF a) = lookup a subs
    f (TypeSeqF seq) = TypeSeq <$> sequenceA seq
    f (TypeConcF conc) = TypeConc <$> sequenceA conc

match :: Eq ident =>
    Type () ident typevar -> 
    Type () ident typevar -> 
    [(ident, Type () ident typevar)]
match = undefined
