{-# LANGUAGE QuasiQuotes #-}
module MplPasses.TypeChecker.KindCheckSpec ( spec ) where

import Optics

import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Text.RawString.QQ

import MplAST.MplCore
import MplPasses.Assertions

import Data.Maybe
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE

import Data.Either
import Data.Traversable

import qualified MplPasses.Parser.BnfcParse as B
import MplPasses.Parser.Parse
import MplPasses.PassesErrors
import MplPasses.Parser.ParseErrors
import MplPasses.Renamer.Rename
import MplPasses.Renamer.RenameErrors
import qualified MplPasses.Renamer.RenameSym as R

import MplPasses.TypeChecker.TypeCheck

import MplPasses.TypeChecker.KindCheck 
import MplPasses.TypeChecker.TypeEqns 
import MplPasses.TypeChecker.TypeCheckSemanticErrors 
import MplPasses.TypeChecker.TypeCheckCallErrors 
import MplPasses.TypeChecker.TypeCheckErrorPkg 
import MplPasses.TypeChecker.TypeCheckMplTypeSub

import MplPasses.Passes
import MplPasses.Env

import Control.Arrow
import System.Directory
import System.FilePath

-- Tests for overlapping declarations and out of scope errors 
spec :: SpecWith ()
spec = do
    let casesdir = "test/MplPasses/TypeChecker/cases/kindcheck/"
    poscases <- runIO $ do 
        let poscasesdir = casesdir </> "positive"
        casesdir <- map (poscasesdir</>) <$> listDirectory poscasesdir
        namedcases <- mapM ( sequence . (id &&& readFile) ) casesdir
        return namedcases

    mapM_ describeValidTypeCheck poscases

    negcases <- runIO $ do 
        let negcasesdir = casesdir </> "negativehigherkindedtype"
        casesdir <- map (negcasesdir</>) <$> listDirectory negcasesdir
        namedcases <- mapM ( sequence . (id &&& readFile) ) casesdir
        return namedcases

    mapM_ (`describeAnyErrorsFile` ("higher kinded variable failure", 
            _MplTypeCheckErrors % _TypeCheckKindErrors)
            ) negcases
