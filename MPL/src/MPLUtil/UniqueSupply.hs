{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-cse #-}
module MPLUtil.UniqueSupply where

import System.IO.Unsafe
import Data.IORef
import Control.Arrow

{- Lazy tree of unique values...
 -
 -}

data UniqueSupply = 
    UniqueSupply !Int UniqueSupply UniqueSupply

newtype Unique = Unique Int

uniqueFromUniqueSupply :: UniqueSupply -> Unique 
uniqueFromUniqueSupply (UniqueSupply a _ _) = Unique a

initUniqueSupply :: IORef Int -> IO UniqueSupply
initUniqueSupply ref = unsafeInterleaveIO $ do
    n <- freshInt ref
    l <- initUniqueSupply ref
    r <- initUniqueSupply ref
    return (UniqueSupply n l r)

{-
{-# NOINLINE uniqueIntRef #-}
uniqueIntRef :: IORef Int
uniqueIntRef = unsafePerformIO $ newIORef 0
-}

freshInt :: IORef Int -> IO Int
freshInt ref = atomicModifyIORef' ref (succ&&&id) 
    {-
    do
    n <- readIORef uniqueIntRef
    writeIORef uniqueIntRef (succ n)
    return n
    -}
    

split :: UniqueSupply -> (UniqueSupply, UniqueSupply)
split (UniqueSupply _ l r)= (l, r)

instance Show UniqueSupply where
    show (UniqueSupply n _ _) = show n

instance Show Unique where
    show (Unique n) = show n
