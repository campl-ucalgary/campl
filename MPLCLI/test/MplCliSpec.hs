module MplCliSpec (spec) where

import Test.Hspec
import System.FilePath
import System.Directory
-- Needed to listen for the std output of a program
import System.IO.Silently


import MplCliAssertion 

spec :: Spec 
spec = do
    {- curdir <- runIO $ getCurrentDirectory ; error curdir -}
    collectAndRunTests "test/cases/ifcond"
    collectAndRunTests "test/cases/tupleproj"
    collectAndRunTests "test/cases/list"
    collectAndRunTests "test/cases/data"
    

