{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module MplPasses.Assertions where

import Optics

import Test.Hspec
import Test.QuickCheck
import Test.HUnit

import MplAST.MplPrinter

import qualified MplPasses.Parser.BnfcParse as B
import MplPasses.Parser.Parse
import MplPasses.Parser.ParseErrors
import MplPasses.Renamer.Rename
import MplPasses.Renamer.RenameErrors
import qualified MplPasses.Renamer.RenameSym as R
import MplPasses.Passes
import MplPasses.Env

import Control.Monad

describeValidRename prog rst = do
    describe ("Testing the valid program: \n" ++ prog) $ do
        mplpassesenv <- runIO mplPassesEnv
        let prog' = runRename' 
                ( TopLevel
                , mplPassesEnvUniqueSupply mplpassesenv
                , mplPassesContext mplpassesenv )
                <=< runParse' 
                <=< B.runBnfc $ prog
        it "Should be a valid program.." $ do
            case prog' of
                Right prog'' -> do
                    rst prog''
                    return ()
                Left (errs :: [MplPassesErrors]) -> assertFailure (show errs) >> return () 

describeErrors prog (errmsg, pred) = do
    describe ("Testing the invalid program for " ++ errmsg ++ ":\n" ++ prog) $ do
        env <- runIO mplPassesEnv
        let res = runPasses env prog

        it ("Testing for " ++ errmsg ++ " error.") $ do
            case res of
                Right prg -> assertFailure $ "Program is valid when it should not be... \n "++ pprint prg
                Left errs -> assertBool ("Expected " ++ errmsg ++ " but got " ++ show errs)
                    $ pred errs

describeAllErrors prog (errmsg, prism) = describeErrors prog (errmsg, allOf folded (has prism))
describeAnyErrors prog (errmsg, prism) = describeErrors prog (errmsg, anyOf folded (has prism))
