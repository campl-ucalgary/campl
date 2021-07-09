{-# LANGUAGE QuasiQuotes #-}
module Suite.DecToZero (decToZeroBench) where

import Criterion

import MplCliGen
import MplCliGenTH 
import MplCliBenchRunner

import Data.List

import qualified Data.Map as Map

import Control.Arrow

decToZeroBench :: Benchmark
decToZeroBench = bgroup "decToZero" $ 
    metaProgSubsAndProgToBenchmarks 
        subs
        dectozerometaprog 
  where
    subs = take 5 
        $ map (uncurry Map.singleton <<< MetaVal *** show) 
        $ iterate (second (10*)) ("n", 10)
        

dectozerometaprog :: MetaProg
dectozerometaprog = [metaprog| 
coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

fun decToZero :: Int -> [Char] =
    0 -> "wahoo!"
    n -> decToZero(n - 1)

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put decToZero( ??n ) on _console

        hput ConsoleClose on _console
        halt _console
|]
