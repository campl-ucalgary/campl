{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module MplParse.MplParseSpec where

import Optics

import Control.Arrow

import Test.Hspec
import Test.QuickCheck
import Test.HUnit

import MplParse.MplParse
import MplParse.Stack
import Text.RawString.QQ

import MplParse.Assertions
import Data.Foldable
import Data.Traversable

import System.Directory
import System.FilePath

spec :: SpecWith ()
spec = do
    -- *  pMplDataClauseSpine tests
    let casesdir = "test/MplParse/cases/pMplDataClauseSpine"
    negcases <- runIO $ do 
        let negcasesdir = casesdir </> "negative"
        casesdir <- map (negcasesdir</>) <$> listDirectory negcasesdir
        namedcases <- mapM ( sequence . (id &&& readFile) ) casesdir
        return namedcases

    poscases <- runIO $ do 
        let poscasesdir = casesdir </> "positive"
        casesdir <- map (poscasesdir</>) <$> listDirectory poscasesdir
        namedcases <- mapM ( sequence . (id &&& readFile) ) casesdir
        return namedcases

    describe ("pMplDataClauseSpine" ++ show (map fst poscases))$ 
        forM_ poscases $ \eg -> it (fst eg ++ " should parse") $ 
            pMplDataClauseSpine `shouldParse` snd eg

    describe ("Negative pMplDataClauseSpine: " ++ show (map fst negcases)) $ 
        forM_ negcases $ \neg -> it (fst neg ++ " should NOT parse") $ 
            pMplDataClauseSpine `shouldNotParse` snd neg
    return ()
