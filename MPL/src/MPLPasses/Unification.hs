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
    TypeEqnsEq (TypeEqns ident, TypeEqns ident)
    | TypeEqnsExist [ident] [TypeEqns ident]
    | TypeEqnsForall [ident] [TypeEqns ident]

$(concat <$> traverse makeBaseFunctor 
    [ ''TypeEqns ]
 )
$(concat <$> traverse makePrisms 
    [ ''TypeEqns 
    ]
 )

match :: Eq ident =>
    Type () ident -> 
    Type () ident -> 
    [(ident, Type () ident)]
match = undefined
