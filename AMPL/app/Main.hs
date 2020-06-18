module Main where

import System.Environment
import AMPL

main :: IO ()
main = do
    args <- getArgs
    case args of
        [input] -> do
            initmachstate <- read <$> readFile input
            execAmplMachWithDefaultsFromInitAMPLMachState "5000" initmachstate
        [input, port] -> do
            initmachstate <- read <$> readFile input
            execAmplMachWithDefaultsFromInitAMPLMachState port initmachstate
        _ -> error "Invalid input file"

