module MplCliAssertion where

import Test.Hspec
import Test.HUnit
import System.FilePath
import Data.Maybe

import MplCliRunner.Runner
import System.IO.Error
import Data.Traversable
import Data.Foldable

import Text.Parsec
import Text.Parsec.Char

import Control.Monad.Except

import System.Timeout

import qualified Data.Bifunctor as Bi

-- Needed to listen for the std output of a program
import qualified System.IO.Silently

import System.Directory


{- | asserts that an mpl file outputs what it should.  Note that this assumes a default timeout of 5 seconds.  -}
assertMplFile :: 
    FilePath -> 
    Assertion 
assertMplFile = assertMplFileTimeout (5 * 1000000) 

assertMplFileTimeout :: 
    -- | time in microseconds. a second is 1/1000000 microseconds
    Int ->  
    -- | file path of file
    FilePath -> 
    Assertion 
assertMplFileTimeout tm fp = do
    inp <- readFile fp
    sol <- liftEither $ Bi.first (userError . show) $ runParser pSolution () fp inp

    res <- timeout tm $ System.IO.Silently.capture_ $ cliRunner ["--run", fp]

    when (isNothing res) $ assertFailure "timed out"

    sol @=? fromJust res

{- | This will collect all tests in a directory, and run them -}
collectAndRunTests :: 
    FilePath ->
    Spec 
collectAndRunTests fp = describe fp 
    $ runIO (listDirectory fp)
        >>= mapM_  (\fl -> let fl' = fp </> fl in it fl' $ assertMplFile fl')
    



-- | trivial type wrapper for Parsec
type MplCliSolutionParser = Parsec String ()

{- | `pSolution` will parse the solution out of the comment of an mpl file.
Explicitly, we expect (and parse)
    - the top most string should be a multiline multiline comment
   
    - anything after the line which opens  the multi line comment
        is part of the solution string

    - and the solution string is all lines until the corresponding closing multi line
        comment is reached

N.B. This does no effort to allow the solution string to include the closing multiline comment
string. Test cases should NOT include this.
    
For example, an example test case could be written as follows.
@
{-
3
-}

coprotocol S => Console =
    ConsolePut :: S => Get( [Char] | S) 
    ConsoleGet :: S => Put( [Char] | S) 
    ConsoleClose :: S => TopBot 

proc run :: | Console => =
    | _console => -> do
        hput ConsolePut on _console
        put "3" on _console
        hput ConsoleClose on _console
        halt _console
@
and this would parse the string "3\n" as the solution.
-}
pSolution :: Parsec String () String
pSolution = spaces 
    *> string "{-" 
    *> manyTill anyChar (char '\n')
    -- (alternative way to skip the line) skipMany (noneOf "\n") *> newline
    *> manyTill anyChar (try $ string "-}")



