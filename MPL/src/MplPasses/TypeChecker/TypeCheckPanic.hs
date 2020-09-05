{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module MplPasses.TypeChecker.TypeCheckPanic where


import Data.Data

import Optics
import Optics.State.Operators
import qualified Data.Map as Map

import Control.Monad.State
import Control.Monad.Writer

panicSymTab :: a
panicSymTab = error "Illegal symbol table lookup -- most likely a thunk was evaluated too early."

panicNotImplemented :: a
panicNotImplemented = error "Not implemented yet...."


panicDeprecated :: a
panicDeprecated = error "Sorry! This was deprecated..."

{-
data MyStrange = MyStrange {
    _innertuple :: (Int,Int)
}  deriving Show

$(makeClassy ''MyStrange)
    
fun :: 
    ( MonadWriter String m0
    , MonadWriter String m1
    , MonadWriter String n
    , Zoom m0 n (Int, Int) MyStrange 
    , Zoom m1 n Int MyStrange 
    ) =>
    n Int
fun = do
    innertuple % _1 %= succ
    {-
    huh <- zoom innertuple $ do
        tell "a"
        _1 %= succ
        -}
    huh <- zoom innertuple $ wowcool
    huh <- zoom (innertuple % _1) $ do
        tell "b"
        -- guses equality succ
        equality %= succ
        guse equality
    tell "c"
    return huh

wowcool = do
    tell "a"
    _1 %= succ

callme = fun >> return ()

data MyStrange = MyStrange {
    _innertuple :: (Int,Int)
    , _innermap :: Map.Map Int (Either Int Int)
}  deriving Show

$(makeLenses ''MyStrange)

mytestfun = do
    zoomMaybe (innermap % at 1 % _Just ) $ do
        guse equality
    -}



fkingaround :: MonadState Int m => m Int -> StateT Int m Int
fkingaround act = do
    equality %= succ
    lift act
    
    

