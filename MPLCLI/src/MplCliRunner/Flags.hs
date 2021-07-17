{-# LANGUAGE TemplateHaskell #-}
module MplCliRunner.Flags where

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

{- | runs a dump output -}
dumpOutput :: 
    DumpOutput ->
    String ->
    IO ()
dumpOutput opt str =  case opt of
    DumpToFile fp -> writeFile fp str
    DumpToStdOut -> putStrLn str

data DumpOpt
    = Parsed
    | Renamed
    | TypeChecked
    | PatternCompiled
    | LambdaLifted
    | Assembled
    | AssembledAst
  deriving (Show, Eq, Enum)

-- | List of all dump options.
allDumpOpts :: [DumpOpt]
allDumpOpts = [Parsed .. AssembledAst]

-- | Converts a dumped opt to a String for printing
-- for generating the command line operations
dumpOptShowOptions :: DumpOpt -> String
dumpOptShowOptions dumpopt = case dumpopt of
    Parsed -> "parsed"
    Renamed -> "renamed"
    TypeChecked -> "type-checked"
    PatternCompiled -> "pattern-compiled"
    LambdaLifted -> "lambda-lifted"
    Assembled -> "assembled"
    AssembledAst -> "assembled-ast"

data Flag
    = Inp FilePath
    | Dump DumpOpt DumpOutput

    | RunMplMach
    | MachHostName String
    | MachPort String
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

-- | gets the options from a list of the command line arguments
getOpts :: [String] -> IO ([Flag], FilePath)
getOpts args = case GetOpt.getOpt GetOpt.RequireOrder options args of
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
    options = map mkDdump allDumpOpts ++
        [ GetOpt.Option 
            ['r']
            ["run"]
            ( GetOpt.NoArg RunMplMach )
            "run the abstract machine with the provided program"
        {-
        , GetOpt.Option
            ['h']
            ["hostname"]
            ( GetOpt.ReqArg 
                (\hn -> MachHostName hn 
                "localhost"
                )
                "hostname used for the machine internally"
            )
        , GetOpt.Option
            ['p']
            ["port"]
            ( GetOpt.ReqArg 
                (\hn -> MachPort hn 
                "5000"
                )
                "port number used for the machine internally"
            )
        -}
        ]

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





