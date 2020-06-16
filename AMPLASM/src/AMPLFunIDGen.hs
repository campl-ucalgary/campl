{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module AMPLFunIDGen where

import AMPLTypes
import Control.Monad.MonadUnique
import Control.Monad.State

class AMPLFunIDGen s where
    succFunID :: s -> s
    initFunID :: s 
    getFunID :: s -> FunID

instance (AMPLFunIDGen s, Monad m) => MonadUnique s (UniqueT s m) where
    fresh = UniqueT $ unUniqueT $ do
        n <- get
        put (succFunID n)
        return n

instance AMPLFunIDGen FunID where 
    succFunID = succ
    initFunID = FunID 0
    getFunID = id
    
