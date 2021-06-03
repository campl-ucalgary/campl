{-# LANGUAGE TemplateHaskell #-}
module Config where

import Optics
import System.FilePath

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import qualified System.Console.GetOpt as GetOpt

import System.IO
import System.Environment
import Control.Exception
import Data.List

data DumpOutput
    = DumpToFile FilePath
    | DumpToStdOut 
  deriving (Show, Eq)

data DumpOpt
    = Parsed
    | Renamed
    | TypeChecked
    | PatternCompiled
    | LambdaLifted
    | Asm
  deriving (Show, Eq, Enum)

-- | List of all dump options.
allDumpOpts :: [DumpOpt]
allDumpOpts = [Parsed .. Asm]

-- | Converts a dumped opt to a String for printing
-- for generating the command line operations
dumpOptShowOptions :: DumpOpt -> String
dumpOptShowOptions dumpopt = case dumpopt of
    Parsed -> "parsed"
    Renamed -> "renamed"
    TypeChecked -> "type-checked"
    PatternCompiled -> "pattern-compiled"
    LambdaLifted -> "lambda-lifted"
    Asm -> "asm"

data Flag
    = Inp FilePath
    | Out FilePath
    | Dump DumpOpt DumpOutput
  deriving (Show, Eq)

dump :: DumpOpt -> Maybe FilePath -> Flag
dump opt = maybe (builddump DumpToStdOut) (builddump . DumpToFile)
  where
    builddump = Dump opt

$(makePrisms ''Flag)
$(makePrisms ''DumpOutput)
$(makePrisms ''DumpOpt)


-- | The exception thrown
newtype GetOptsException = GetOptsException String

instance Show GetOptsException where
    show (GetOptsException str) = concat 
        [ "command line args error:"
        , "\n"
        , str
        ]
instance Exception GetOptsException where

-- | gets the options from the command line arguments
getOpts :: IO ([Flag], FilePath)
getOpts = getArgs >>= \args -> case GetOpt.getOpt GetOpt.RequireOrder options args of
    -- (flags, nonoptions@[_], []) -> return (flags, nonoptions)
    (flags, [nonoptions], []) -> return (flags, nonoptions)
    (_, _, errs) -> 
        throwIO
        $ GetOptsException
        $ concat 
            [ concat errs 
            , GetOpt.usageInfo header options
            ]
  where
    header :: String
    header = "Usage: mpl [OPTIONS ...] FILE"

    options :: [GetOpt.OptDescr Flag]
    options = 
        map mkDdump allDumpOpts
      where
        mkDdump dmpopt = GetOpt.Option 
            []
            [flag]
            (GetOpt.OptArg (dump dmpopt) "output")
            descr
          where
            -- e.g. @ddump-parsed@, .. etc.
            flag = "ddump-" ++ dumpOptShowOptions dmpopt
            -- description
            descr = intercalate " " ["dump", dumpOptShowOptions dmpopt, "to output"]





