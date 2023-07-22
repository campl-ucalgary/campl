
module MplCliRunner.Modules.ListOps  (makeModuleList) where

-- For filesystem interactions
import System.Directory
-- For parsing new files
import MplPasses.Parser.BnfcParse as B
-- For lifting things to be compatible with the MplCLi type.
import Control.Monad.IO.Class
-- For the MplCli type, which is the monad type for general-purpose errors.
import MplCliRunner.Stack

-- this module handles list operations on lists of modules.
-- This is the main file used for the 'include modules' step of the compiler
-- TODO all of this.

-- A list of module objects
type ModuleList = [Module]

-- A file path, AST, and list of dependencies: (File path, local name)
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
makeModuleList ast path =
    


-- Takes an AST, and turns it into a Module entry.
-- Converts relative directories to absolute directories.
-- 'Fills in' any omitted details in the individual imports,
-- making the AST easier to work with.
-- Can fail, either because of an OS error (can't find directories) or because
-- there's an aliasing error (since the imports are just aliasing)
generateModule :: B.MplProg -> String -> IO Module


-- turns single or double back-slashes into forward slashes.
-- turns any empty string into "."
cleanDir :: String -> String

-- splits a directory into its path and filename.
-- Assumes path uses forward slashes, not back-slashes.
-- If the path is empty, uses '.' instead.
splitDir :: String -> (String,String)