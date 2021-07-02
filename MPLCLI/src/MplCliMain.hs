module MplCliMain where

import MplCliRunner.Runner
import System.Environment

mainCliRunner :: IO ()
mainCliRunner = getArgs >>= cliRunner
