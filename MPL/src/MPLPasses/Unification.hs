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



data TypeEqns ident =
    TypeEqnsEq (Type (TypeClauseG ident) ident, Type (TypeClauseG ident) ident)
    | TypeEqnsExist [ident] [TypeEqns ident]
    | TypeEqnsForall [ident] [TypeEqns ident]

$(concat <$> traverse makeBaseFunctor 
    [ ''TypeEqns ]
 )
$(concat <$> traverse makePrisms 
    [ ''TypeEqns 
    ]
 )

substitutes :: Eq ident =>
    [(ident, Type (TypeClauseG ident) ident)] -> 
    Type (TypeClauseG ident) ident -> 
    Type (TypeClauseG ident) ident 
substitutes subs ty = foldr substitute ty subs

substitute :: Eq ident =>
    (ident, Type (TypeClauseG ident) ident) -> 
    Type (TypeClauseG ident) ident -> 
    Type (TypeClauseG ident) ident
substitute (v, sub) = cata f
  where
    f (TypeVarF ident) 
        | v == ident = sub
        | otherwise = TypeVar ident
    f n = embed n

match :: Eq ident =>
    Type () ident -> 
    Type () ident -> 
    [(ident, Type () ident)]
match = undefined
