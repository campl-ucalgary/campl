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
import MplPasses.PassesErrors
import MplPasses.Parser.ParseErrors
import MplPasses.Renamer.Rename
import MplPasses.Renamer.RenameErrors
import qualified MplPasses.Renamer.RenameSym as R
import MplPasses.TypeChecker.TypeCheck
import MplAST.MplCore

import MplPasses.Passes
import MplPasses.Env
import MplUtil.UniqueSupply

import Data.Proxy

import Control.DeepSeq
import Control.Monad
import System.FilePath

describeValidRename prog rst = do
    describe ("Testing the valid program: \n" ++ prog) $ do
        mplpassesenv <- runIO mplPassesEnv
        let 
            prog' = runRename' ( mplPassesTopLevel mplpassesenv , mplPassesEnvUniqueSupply mplpassesenv )
                <=< runParse' 
                <=< B.runBnfc $ prog
        it "Should be a valid program.." $ do
            case prog' of
                Right prog'' -> do
                    rst prog''
                    return ()
                Left (errs :: [MplPassesErrors]) -> assertFailure (show errs) >> return () 

describeValidTypeCheck :: 
    -- | file name, and file input 
    (FilePath, String) -> 
    SpecWith ()
describeValidTypeCheck inp = describeValidTypeCheckWithContinuation inp k
  where
    -- we do this to check if the output returns any exceptions
    k :: MplProg MplTypeChecked -> IO () 
    k prog = pprint (Proxy :: Proxy MplRenamed) prog `deepseq` return ()

describeValidTypeCheckWithContinuation :: 
    (FilePath, String) -> 
    (MplProg MplTypeChecked -> IO a) -> 
    SpecWith ()
describeValidTypeCheckWithContinuation (filename, prog) rst = do
    describe ("This should be a valid program: " ++ filename) $ do
        mplpassesenv <- runIO mplPassesEnv
        let (ls, rs) = split $ mplPassesEnvUniqueSupply mplpassesenv
            prog' =
                runTypeCheck' ( mplPassesTopLevel mplpassesenv , rs)
                <=< runRename' ( mplPassesTopLevel mplpassesenv , ls)
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
                Right prg -> assertFailure 
                    $ "Program is valid when it should not be... \n "++ pprint (Proxy :: Proxy MplRenamed) prg
                Left errs -> 
                    assertBool ("Expected " ++ errmsg ++ " but got " ++ show errs)
                    $ pred errs

describeAllErrors prog (errmsg, prism) = describeErrors prog (errmsg, allOf folded (has prism))

describeAnyErrors prog (errmsg, prism) = describeErrors prog (errmsg, anyOf folded (has prism))


describeErrorsFile (filename, prog) (errmsg, pred) = do
    describe ("Testing the invalid program for " ++ errmsg ++ " of " ++ filename) $ do
        env <- runIO mplPassesEnv
        let res = runPasses env prog

        it ("Testing for " ++ errmsg ++ " error.") $ do
            case res of
                Right prg -> assertFailure 
                    $ "Program is valid when it should not be... \n "++ pprint (Proxy :: Proxy MplRenamed) prg
                Left errs -> 
                    assertBool ("Expected " ++ errmsg ++ " but got " ++ show errs)
                    $ pred errs

describeAnyErrorsFile prog (errmsg, prism) = 
    describeErrorsFile prog (errmsg, anyOf folded (has prism))
