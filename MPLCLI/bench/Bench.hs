module Main where

import Criterion.Main

import Suite.DecToZero

{- Notes on actually running the benchmarks:
Type
@
stack bench
@
to run the benchmarks and it will output some results to console.

Type
@
stack bench --benchmark-arguments '--output nameofhtml.html'
@
to output the benchmarks to console and output graphs in the html page.

-}


main :: IO ()
main = defaultMain 
    [ decToZeroBench 
    ]

