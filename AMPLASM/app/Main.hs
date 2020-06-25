module Main where

import AMPLASMMain
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let conf = case args of
                [input] -> AmplAssembleConfig input Nothing
                [input, output] -> AmplAssembleConfig input (Just output)
    amplAssembleMain conf

