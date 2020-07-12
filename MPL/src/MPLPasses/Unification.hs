{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module MPLPasses.Unification where

import MPLAST.MPLASTCore
import MPLAST.MPLTypeAST

import Optics
import Data.Functor.Foldable
import Data.Functor.Foldable.TH



data TypeEqns ident typevar =
    TypeEqnsEq (Type (TypeClauseG ident) ident typevar, Type (TypeClauseG ident) ident typevar)
    | TypeEqnsExist [ident] [TypeEqns ident typevar]
    | TypeEqnsForall [ident] [TypeEqns ident typevar]

$(concat <$> traverse makeBaseFunctor 
    [ ''TypeEqns ]
 )
$(concat <$> traverse makePrisms 
    [ ''TypeEqns 
    ]
 )

substitutes subs ty = foldr substitute ty subs

substitute (v, sub) = cata f
  where
    f (TypeVarF ident) 
        | v == ident = sub
        | otherwise = TypeVar ident
    f n = embed n

match :: Eq ident =>
    Type () ident typevar -> 
    Type () ident typevar -> 
    [(ident, Type () ident typevar)]
match = undefined
