{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-cse #-}
module MplUtil.UniqueSupply where

import Optics
import Optics.State.Operators

import System.IO.Unsafe
import Data.IORef
import Control.Arrow
import Data.Word

import MplUtil.Data.Stream (Stream (..))
import qualified MplUtil.Data.Stream as Stream

import Control.Monad.State

import Debug.Trace
import Data.Ix


{- Lazy tree of unique values... -}

data UniqueSupply = 
    UniqueSupply !Word UniqueSupply UniqueSupply
$(makeClassy ''UniqueSupply)

newtype Unique = Unique Word
  deriving (Show, Eq, Ord, Ix, Read, Enum)

uniqueFromSupply :: UniqueSupply -> Unique 
uniqueFromSupply ~(UniqueSupply a _ _) = Unique a

uniquesFromSupply :: 
    UniqueSupply -> 
    Stream Unique
uniquesFromSupply supply = 
    uniqueFromSupply supply 
        :/ uniquesFromSupply r
  where
    ~(_,r) = split supply

initUniqueSupply :: Word -> IO UniqueSupply
initUniqueSupply seed = initUniqueSupply' =<< newIORef seed

initUniqueSupply' :: IORef Word -> IO UniqueSupply
initUniqueSupply' ref = unsafeInterleaveIO $ do
    n <- freshWord ref
    l <- initUniqueSupply' ref
    r <- initUniqueSupply' ref
    return (UniqueSupply n l r)

{-
{-# NOINLINE uniqueIntRef #-}
uniqueIntRef :: IORef Int
uniqueIntRef = unsafePerformIO $ newIORef 0
-}

freshWord :: IORef Word -> IO Word
freshWord ref = atomicModifyIORef' ref (succ&&&id) 
    {-
    do
    n <- readIORef uniqueIntRef
    writeIORef uniqueIntRef (succ n)
    return n
    -}
    

split :: UniqueSupply -> (UniqueSupply, UniqueSupply)
split ~(UniqueSupply _ l r)= (l, r)


instance Show UniqueSupply where
    show (UniqueSupply n _ _) = "UniqueSupply " ++ show n

{-
instance Show Unique where
    show (Unique n) = show n
    -}

freshUniqueSupply ::
    ( MonadState c m
    , HasUniqueSupply c ) => 
    m UniqueSupply
freshUniqueSupply =
    uniqueSupply %%= split

-- this actually does nothing.
splitUniqueSupply ::
    ( MonadState c m
    , HasUniqueSupply c ) => 
    m a -> m a
splitUniqueSupply action = do
    uniqsupply <- freshUniqueSupply
    res <- action
    uniqueSupply .= uniqsupply
    return res
