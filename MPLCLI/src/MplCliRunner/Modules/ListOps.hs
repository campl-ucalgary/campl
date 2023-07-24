
module MplCliRunner.Modules.ListOps (makeModuleList,orderModules,uniqueModNames,resolveList) where

-- For filesystem navigation
import System.Directory
-- For file path manipulations
import System.FilePath
-- For parsing new files and the AST definitions
import MplPasses.Parser.BnfcParse as B
-- For lifting things to be compatible with the MplCli type.
import Control.Monad.IO.Class
-- For the MplCli type, which is the monad type for general-purpose errors.
import MplCliRunner.Stack
-- For errors
import Data.Either
-- For liftEither
import Control.Monad.Except
-- for aliasing operations
import MplCliRunner.Modules.Aliasing
-- for refactoring ASTs
import MplCliRunner.Modules.Renamer
-- for error messages
import MplCliRunner.Modules.ErrorHandling
-- for the file reading
import qualified Data.Bifunctor as Bifunctor
-- for file parsing errors
import qualified MplPasses.PassesErrors as PassesErrors


-- this module handles list operations on lists of modules.
-- This is the main file used for the 'include modules' step of the compiler
-- TODO all of this.

-- A file path, AST, and list of dependencies: (Full file path, local name)
-- The file paths should all be absolute paths, since file equality is being checked.
-- The 'local name' of a dependency is the '?' found in "?.object" references in MPL programs.
type Module = (String,B.MplProg,[(String,String)])

-- Generates a list of modules, starting with a single module.
-- This requires importing and parsing several new files,
-- which can potentially produce errors.
-- (circular dependency errors, namespace errors, object conflicts, etc)
makeModuleList :: B.MplProg -> String -> MplCli [Module]
makeModuleList ast file = do
    m <- generateModule ast file
    genFullList [m] -- Build list until all dependencies are satisfied.


-- Takes an AST and full file address, and turns it into a Module entry.
-- Converts relative directories to absolute directories.
-- 'Fills in' any omitted details in the individual imports,
-- making the AST easier to work with.
-- Can fail, either because of an OS error (directory libraries) or because
-- there's an aliasing error (since the imports are just aliasing)
generateModule :: B.MplProg -> String -> MplCli Module
generateModule ast dir = do
    -- Get a full file directory, nicely formatted.
    fullDir <- liftIO $ makeAbsolute $ cleanDir dir
    -- get list of aliases 
    aliasLists <- return $ defProg ast
    -- get list of local names
    (impList,newAst) <- return $ modListProg ast
    -- Get the path of the item, so that we can temporarily change
    -- the current directory.
    (fullPath,_) <- return $ splitFileName fullDir
    -- add the full paths to the import list name
    impListQ <- liftIO
        $ withCurrentDirectory fullPath -- temporarily change the current directory while working with relative dirs
        $ cleanLocals impList
    -- check for imports in the wrong places
    liftEither $ moduleInsideWhere $ findBadProg newAst
    -- check for local-name conflicts
    liftEither $ checkLocalClash impListQ
    -- check for alias conflicts
    liftEither $ checkForAliasClash aliasLists
    -- remove aliases
    finalAst <- return $ deAliasAll aliasLists newAst
    -- Return the completed structure.
    return (fullDir,finalAst,impListQ)


-- Takes a raw list of module local-names and makes them global.
cleanLocals :: [(String,String)] -> IO [(String,String)]
cleanLocals [] = return []
cleanLocals ((dir,name):bs) = do
    -- Clean up this directory.
    dir2 <- liftIO $ makeAbsolute $ cleanDir dir
    -- Clean up the rest
    b2s <- cleanLocals bs
    -- Return the result
    return ((dir2,name):b2s)


-- Turns double back-slashes into single back-slashes,
-- silently ignores quotation marks.
-- Since this is the 'string' standard for the grammer, we have to manually fix this.
cleanDir :: String -> String
cleanDir [] = []
cleanDir ('"':ss) = cleanDir ss -- ignore double quotes
cleanDir ('\\':'\\':ss) = '\\':(cleanDir ss) -- double to single back-slashes
cleanDir (s:ss) = s : (cleanDir ss)



------------------------------------------------------
------------------------------------------------------
-- Part 1: Generating the full list of modules
------------------------------------------------------
------------------------------------------------------

genFullList :: [Module] -> MplCli [Module]
genFullList ls = do
    stillOutstanding <- return $ getMissingFiles ls 
    outstandingLoaded <- loadFiles stillOutstanding
    if stillOutstanding == []
        then return ls -- We are done
        else genFullList (ls ++ outstandingLoaded) -- keep building.


-- Takes a list of modules and generates a list of missing dependencies
getMissingFiles :: [Module] -> [String]
getMissingFiles [] = []
getMissingFiles ms =
    case (allModuleNames ms) of
        (existing,new) -> setify $ listSubtract new existing

-- Takes a list of modules and returns a lists of existing and new module file paths
allModuleNames :: [Module] -> ([String],[String])
allModuleNames [] = ([],[])
allModuleNames ((name,_,ls):ms) =
    case (allModuleNames ms) of
        (ex,ne) -> (name:ex, [x | (x,_) <- ls] ++ ne)


-- Removes repeated entries from a list.
setify :: Eq a => [a] -> [a]
setify [] = []
setify (n:ns) = n:(setify [x | x <- ns, x /= n])

-- removes entries in the second list from the first, and returns the first.
listSubtract :: Eq a => [a] -> [a] -> [a]
listSubtract bs [] = bs
listSubtract bs (c:cs) = listSubtract [x | x <- bs, x /= c] cs


-- Takes a list of full directories and loads them, one-by-one
-- Can fail for a wide variety of reasons.
-- (OS errors, parsing errors, aliasing errors and other name errors)
loadFiles :: [String] -> MplCli [Module]
loadFiles [] = return [] -- List complete
loadFiles (n:ns) = do
    -- Read the file
    rawStr <- liftIO $ readFile n
    -- parse the file (stolen from the cliRunPipelineInputProg implementation)
    ast <- liftEither 
        $ Bifunctor.first 
            ( ParsedException 
            . show 
            . (PassesErrors.pprintMplPassesErrors :: [PassesErrors.MplPassesErrors] -> PassesErrors.MplDoc) 
            )
        $ B.runBnfc 
        $ rawStr
    -- Generate the module, in full.
    f <- generateModule ast n
    fs <- loadFiles ns
    return (f:fs)

------------------------------------------------------
------------------------------------------------------
-- Part 2: Ordering the list of modules by dependency.
------------------------------------------------------
------------------------------------------------------

-- takes a list of modules, (where 'main' is the first module)
-- and orders them in such a way as that a module's dependencies are after itself.
-- (Meaning 'main' should still be the first in the list)
orderModules :: [Module] -> MplCli [Module]
orderModules ls = do
    catchModuleCycles ls -- check for cycles, and print an error if any exist.
    orderModules ls -- orders the modules correctly.
    



-- Checks for a module-module cycle.
-- Assumes the first module in the list is 'main'
-- Returns nothing on success, or an error on failure
catchModuleCycles :: [Module] -> MplCli ()
catchModuleCycles [] = liftIO $ ioError $ userError "bad input to 'catchModuleCycles', fix compiler."
catchModuleCycles (main:rest) = -- Handles the return type of cycle traversal
    case (cycleTraverse main rest) of
        Right () -> return ()
        Left str -> liftEither $ Left $ ModuleException str

-- cycle traversal works as follows:
-- Each step, a module is chosen (main first)
-- the module is removed from the list, and the process is repeated in
-- parallel for every dependency of the removed module.
-- Effectively, we are traversing the graph, removing any node seen before.
-- If it contains cycles, we will be unable to locate a node (because we have seen it before)



-- Takes a module and a list of modules.
-- Recursively checks for problematic dependencies. 
cycleTraverse :: Module -> [Module] -> Either String ()
cycleTraverse (name,_,deps) ms =
    case cycleDepTraverse deps ms of
        Left str -> Left $ cyclePrepend name str
        Right () -> Right ()


-- Looks at a list of dependencies and a list of modules.
-- Tries selecting each dependency as 'main', and recurses the cycleTraverse process.
cycleDepTraverse :: [(String,a)] -> [Module] -> Either String ()
cycleDepTraverse [] _ = Right () -- No more dependencies. All good.
cycleDepTraverse ((d,_):ds) ms =
    case (selectModule d ms) of
        Left str -> Left str
        Right (m,msNew) -> case (cycleTraverse m msNew) of
            Left str -> Left str -- Error found.
            Right () -> cycleDepTraverse ds ms -- Keep looking.


-- takes a list of modules and a name, and finds the module.
-- On success, returns the module and other modules.
-- On failure, returns an error message reflecting that this is the last module in a cycle.
selectModule :: String -> [Module] -> Either String (Module,[Module])
selectModule name [] = Left $ cycleBase name -- error, module cycle detected.
selectModule name (m@(name2,_,_):ms)
    | name == name2 = Right (m,ms) -- module found, return result
    | otherwise = case (selectModule name ms) of
        Right (r,rs) -> Right (r,m:rs) -- prepend module to successful list
        Left str -> Left str -- failure found.


-- Takes a list of modules and orders them so that a module is before its dependencies in the list.
-- (also takes a list of modules that have already been ordered and have all dependencies satisfied)
-- Assumes that an ordering exists (if not, the error should already be thrown)
orderModulesSafe :: [Module] -> [Module] -> [Module]
orderModulesSafe [] rest = rest
orderModulesSafe ms rest =
    case (findGoodMod ms rest) of
        m@(name,_,_) -> -- remove module from the 'to-sort' and add to the 'already-sorted' list.
            orderModulesSafe (removeModule name ms) (m:rest)


-- removes a module from a list (by file name only)
removeModule :: String -> [Module] -> [Module]
removeModule _ [] = []
removeModule name ((name2,ast,deps):rest)
    | name == name2 = removeModule name rest
    | otherwise     = (name2,ast,deps) : (removeModule name rest)

-- Finds a module for which its dependencies are satisfied.
-- Takes a list to search and a list of satisfied dependencies.
findGoodMod :: [Module] -> [Module] -> Module
findGoodMod [] _ = undefined -- Means the cycle wasn't caught by 'catchModuleCycles'
findGoodMod (b:bs) ms =
    if (getMissingFiles (b:ms) == []) -- Means that it can be added without missing dependencies.
        then b -- found the result
        else findGoodMod bs ms -- keep searching


------------------------------------------------------
------------------------------------------------------
-- Part 3: Giving modules unique names.
------------------------------------------------------
------------------------------------------------------


-- Gives modules unique names. Replaces the 'fileName' entries in the modules and dependencies with
-- the unique names.
uniqueModNames :: [Module] -> [Module]
uniqueModNames ms =
    case (generateSubs ms []) of
        subs -> applySubs subs ms


-- generates a list of substitutions to produce unique module names
-- takes a list of modules, and a list of existing substitutions.
-- returns a list of (filename,uniqueName) pairs
generateSubs :: [Module] -> [(String,String)] -> [(String,String)]
generateSubs [] subs = subs
generateSubs ((name,_,_):ms) subs = -- Add a substitution and keep looking.
    generateSubs ms (addSub (name, fileNameOnly name) subs)
    

-- adds a substitution to a list of substitutions.
-- if the name is already claimed, it starts adding 'I' to it until it is free.
addSub :: (String,String) -> [(String,String)] -> [(String,String)]
addSub (file,name) ls =
    if [(f,n) | (f,n) <- ls, n == name] /= [] -- if the name is taken
        then addSub (file,name++"I") ls -- try again with an extra "I"...
        else (file,name):ls -- otherwise, we can safely add it.
    

-- Extracts just a fileName from a file path string
fileNameOnly :: String -> String 
fileNameOnly str =
    case (splitFileName str) of
        (_,fileOnly) -> dropExtensions fileOnly


-- Applies substitutions to the module structure
-- (does not act on ASTs)
applySubs :: [(String,String)] -> [Module] -> [Module]
applySubs _ [] = []
applySubs subs ((name,ast,deps):rest) =
    (applySub name subs, ast,
        [(applySub n subs, loc) | (n,loc) <- deps])
        : (applySubs subs rest)


-- Applies a single substitution to a single string.
applySub :: String -> [(String,String)] -> String
applySub a [] = undefined -- error, substitution should exist.
applySub a ((b,bNew):bs)
    | a == b    = bNew
    | otherwise = applySub a bs


------------------------------------------------------
------------------------------------------------------
-- Part 4: Reducing a module list to a single file
------------------------------------------------------
------------------------------------------------------

-- Takes a module list. Assumes the first module is the main module,
-- and modules are ordered such that any module is before its dependencies.
resolveList :: [Module] -> B.MplProg
resolveList ms =
    concatProgs $ reverse $ globalNameEverything ms


-- concatenates programs
concatProgs :: [B.MplProg] -> B.MplProg
concatProgs [] = MPL_PROG []
concatProgs ((MPL_PROG l1s):bs) =
    case (concatProgs bs) of
        MPL_PROG l2s -> MPL_PROG (l1s++l2s)

-- takes a list of modules, same input as 'resolveList', and reduces it to a list of programs
-- resolves local-name global-name substitutions,
-- and converts every module except 'main' to its actual name, by refactoring definitions and uses
-- of local functions (for example, f -> M.f)
globalNameEverything :: [Module] -> [B.MplProg]
globalNameEverything [] = undefined -- means there is no main function
globalNameEverything ((_,ast,subs):ms) = -- the first module is a special case
    (astRefactor -- turns 'Local_M.f' to 'Global_M.f'
        Left
        (globalizeModName subs) -- refactor module names
        Left
        (globalizeModName subs) -- refactor module names
        ast
        )
    : (globalNameRest ms)

-- same as globalNameEverything, but also turns things into modules.
globalNameRest :: [Module] -> [B.MplProg]
globalNameRest [] = []
globalNameRest ((modName,ast,subs):ms) =
    (refToModule modName -- changes every 'f' to 'M.f' in accordance with the global module name
        (astRefactor -- turns 'Local_M.f' to 'Global_M.f'
            Left
            (globalizeModName subs) -- refactor module names
            Left
            (globalizeModName subs) -- refactor module names
            ast
            )
    ) : (globalNameRest ms)


-- takes a list of subs and reverses the direction (switches the elements)
subDirRev :: [(a,b)] -> [(b,a)]
subDirRev ls = [(b,a) | (a,b) <- ls]


-- takes a local module name and substitutes it for the global version.
globalizeModName :: [(String,String)] -> (String,String) -> Either String (String,String)
globalizeModName subs (m,o) =
    Right (applySub m (subDirRev subs),o)