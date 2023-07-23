
module MplCliRunner.Modules.ListOps  (makeModuleList) where

-- For filesystem navigation
import System.Directory
-- For file path manipulations
import System.FilePath
-- For parsing new files and the AST definitions
import MplPasses.Parser.BnfcParse as B
-- For lifting things to be compatible with the MplCLi type.
import Control.Monad.IO.Class
-- For the MplCli type, which is the monad type for general-purpose errors.
import MplCliRunner.Stack
-- For errors
import Data.Either
-- For MplCli types
import qualified MplCliRunner.Stack as Stack
-- For liftEither
import Control.Monad.Except

-- this module handles list operations on lists of modules.
-- This is the main file used for the 'include modules' step of the compiler
-- TODO all of this.

-- A list of module objects
type ModuleList = [Module]

-- A file path, AST, and list of dependencies: (Full file path, local name)
-- The file paths should all be absolute paths, since file equality is being checked.
-- The 'local name' of a dependency is the '?' found in "?.object" references in MPL programs.
type Module = (String,B.MplProg,[(String,String)])

-- A list of simplified modules
type SimpleModuleList = [SimpleModule]

-- This is a simplified representation of a module.
-- The strings reference 'canonical names' of modules, which are unique string identifiers for modules.
-- The file paths were being used for identifying uniqueness, and since there are now unique names,
--  we don't track that anymore.
type SimpleModule = (String,B.MplProg,[String])

-- Generates a list of modules, starting with a single module.
-- This requires importing and parsing several new files,
-- which can potentially produce errors.
-- (circular dependency errors, namespace errors, and object conflicts handled later)
makeModuleList :: B.MplProg -> String -> MplCli ModuleList
makeModuleList ast path = return [] -- TODO
    


-- Takes an AST and full file address, and turns it into a Module entry.
-- Converts relative directories to absolute directories.
-- 'Fills in' any omitted details in the individual imports,
-- making the AST easier to work with.
-- Can fail, either because of an OS error (directory libraries) or because
-- there's an aliasing error (since the imports are just aliasing)
generateModule :: B.MplProg -> String -> MplCli Module
generateModule ast dir = do
    
    
    return 
    -- TODO
    

-- Turns double back-slashes into single back-slashes,
-- silently ignores quotation marks.
-- Since this is the 'string' standard for the grammer, we have to manually fix this.
cleanDir :: String -> String
cleanDir [] = []
cleanDir ('"':ss) = cleanDir ss -- ignore double quotes
cleanDir ('\\':'\\':ss) = '\\':(cleanDir ss) -- double to single back-slashes
cleanDir (s:ss) = s : (cleanDir ss)