
module MplCliRunner.Modules.Aliasing (
    defProg, -- Extracts defintions from an AST
    modListProg, -- Extracts a list of dependent modules from a program
    findBadProg, -- Detects unwanted imports
    checkLocalClash, -- Checks if local module names are in conflict
    checkForAliasClash, -- Checks if aliases conflict
    deAliasAll -- Removes all aliases from an AST
    ) where

-- For the AST definitions
import MplPasses.Parser.BnfcParse as B
-- For the FrontEndException definition
import MplCliRunner.Stack
-- For generating error messages.
import MplCliRunner.Modules.ErrorHandling
-- For resolving aliases
import MplCliRunner.Modules.Renamer


-- a module for resolving module aliasing. (the '(f|g)' part of an 'include' statement)
-- Detects aliasing errors (an alias overwrites another alias or definition)



-- TODO all helper functions and main functions.

-- Function aliases, process aliases, function definitions, process definitions
-- Alias format: (objName,modName,lineNum)
-- Defn format: (objName,lineNum)
type AllDefs = ([(String,String,Int)],[(String,String,Int)],[(String,Int)],[(String,Int)])


-- Takes an AST and returns all the object aliases and object declarations
-- (the declarations are to check for aliasing conflicts)
defProg :: B.MplProg -> AllDefs
defProg (MPL_PROG stmts) = defList defStmt stmts

-- a generic def-extractor.
defList :: (a -> AllDefs) -> [a] -> AllDefs
defList _ [] = ([],[],[],[])
defList f (s:ss) =
    catDefs
        (f s)
        (defList f ss)

defStmt :: MplStmt -> AllDefs
defStmt (MPL_DEFN_STMS_WHERE defs _) =
    defList defDef defs
defStmt (MPL_DEFN_STMS defs) =
    defList defDef defs
defStmt (MPL_STMT def) =
    defDef def

defDef :: MplDefn -> AllDefs
defDef (MPL_FUNCTION_DEFN fd) = defFunc fd
defDef (MPL_PROCESS_DEFN pd) = defProc pd
defDef (MPL_IMPORT_DEFN imp) = defImp imp
defDef a = ([],[],[],[]) -- For type definitions, which we ignore.


defFunc :: FunctionDefn -> AllDefs
defFunc (TYPED_FUNCTION_DEFN (PIdent ((line,_),name)) _ _ _) = 
    ([],[],[(name,line)],[])
defFunc (FUNCTION_DEFN (PIdent ((line,_),name)) _) =
    ([],[],[(name,line)],[])
defFunc _ = ([],[],[],[]) -- Ignore infix and internal definitions.

defProc :: ProcessDefn -> AllDefs
defProc (TYPED_PROCESS_DEFN (PIdent ((line,_),name)) _ _) =
    ([],[],[],[(name,line)])
defProc (PROCESS_DEFN (PIdent ((line,_),name)) _) =
    ([],[],[],[(name,line)])
defProc a = ([],[],[],[]) -- internal definitions; ignore

defImp :: ImportDefn -> AllDefs
defImp (IMPORT_DEFN _) = ([],[],[],[])
defImp (IMPORT_DIR_DEFN _ _ _) = ([],[],[],[])
defImp (IMPORT_SPEC_DEFN (UIdent ((line,_),name)) _ funcs procs _) =
    (defAliasList name line funcs, defAliasList name line procs, [], [])
defImp (IMPORT_DIR_SPEC_DEFN _ _ (UIdent ((line,_),name)) _ funcs procs _) =
    (defAliasList name line funcs, defAliasList name line procs, [], [])


-- Takes a module name and line number, and a list of object names,
-- and returns a list of 'alias' entries. Used for parsing import statements.
defAliasList :: String -> Int -> [PIdent] -> [(String,String,Int)]
defAliasList _ _ [] = []
defAliasList name line ((PIdent (_,obj)):rest) =
    (obj,name,line):(defAliasList name line rest)


-- merge two lists of definitions.
catDefs :: AllDefs -> AllDefs -> AllDefs
catDefs (l1a,l2a,l3a,l4a) (l1b,l2b,l3b,l4b) =
    (l1a++l1b,l2a++l2b,l3a++l3b,l4a++l4b)


-----------------------------------------------------------------
-----------------------------------------------------------------
-- Part 2: extracting module names/directories
-----------------------------------------------------------------
-----------------------------------------------------------------

-- Returns a list of (fileName,localName) pairs,
-- reflecting all the modules to import. No validity-checking is done at all.
-- Also returns the AST, with the imports all removed.
-- (Imports inside 'defn' blocks are ignored; we check for these later.)
modListProg :: B.MplProg -> ([(String,String)],B.MplProg)
modListProg (MPL_PROG stmts) =
    case (modListStmts stmts) of
        (ss,ast) -> (ss,MPL_PROG ast)


-- Traverses the list of top-level declarations, finding imports and extracting the (name,filename) pairs.
modListStmts :: [MplStmt] -> ([(String,String)],[MplStmt])
modListStmts [] = ([],[])
modListStmts ((MPL_STMT (MPL_IMPORT_DEFN imp)):bs) =
    case (extractDefn imp) of -- get a new import definition
        s -> case (modListStmts bs) of -- parse the rest of the list
            (ss,ast) -> (s:ss,ast) -- discard the import statement
modListStmts (b:bs) =
    case (modListStmts bs) of -- ignore any 'where' blocks.
        (ss,ast) -> (ss,b:ast)


-- Gets the directory and name from a definition.
extractDefn :: ImportDefn -> (String,String)
extractDefn (IMPORT_DIR_SPEC_DEFN (PString (_,dir)) _ (UIdent (_,name)) _ _ _ _) =
    (dir,name)
extractDefn (IMPORT_DIR_DEFN (PString (_,dir)) _ (UIdent (_,name))) =
    (dir,name)
extractDefn (IMPORT_SPEC_DEFN (UIdent (_,name)) _ _ _ _) =
    (name ++ ".mpl",name)
extractDefn (IMPORT_DEFN (UIdent (_,name))) =
    (name ++ ".mpl",name)

-----------------------------------------------------------------
-----------------------------------------------------------------
-- Part 3: checking for imports inside 'defn' blocks.
-----------------------------------------------------------------
-----------------------------------------------------------------



-- Checks if there is an import statements left in the program.
-- '0' means 'good'
-- Any other number is the line number of the offending statement.
-- (imports should already be removed unless inside a 'defn' block)
findBadProg :: B.MplProg -> Int
findBadProg (MPL_PROG sts) =
    findBadList findBadSt sts

-- Searches a list for the criteria
findBadList :: (a -> Int) -> [a] -> Int
findBadList _ [] = 0
findBadList f (b:bs) = case (f b) of
    0 -> findBadList f bs
    n -> n


findBadSt :: MplStmt -> Int
findBadSt (MPL_DEFN_STMS_WHERE defs whers) =
    case (findBadList findBadDefn defs) of
        0 -> findBadList findBadWher whers
        n -> n
findBadSt (MPL_DEFN_STMS defs) = findBadList findBadDefn defs
findBadSt (MPL_STMT def) = findBadDefn def

findBadWher :: MplWhere -> Int
findBadWher (MPL_WHERE st) = findBadSt st

-- imports are 'bad'. They should have been removed already.
findBadDefn :: MplDefn -> Int
findBadDefn (MPL_IMPORT_DEFN imp) = getImportLine imp
findBadDefn _ = 0 -- otherwise, good.


-- get the line number of an import statement.
getImportLine :: ImportDefn -> Int
getImportLine (IMPORT_DIR_SPEC_DEFN _ _ (UIdent ((line,_),_)) _ _ _ _) = line
getImportLine (IMPORT_DIR_DEFN _ _ (UIdent ((line,_),_))) = line
getImportLine (IMPORT_SPEC_DEFN (UIdent ((line,_),_)) _ _ _ _) = line
getImportLine (IMPORT_DEFN (UIdent ((line,_),_))) = line

-----------------------------------------------------------------
-----------------------------------------------------------------
-- Part 4: checking for alias overwrites
-----------------------------------------------------------------
-----------------------------------------------------------------

-- takes a list of modules and local names, checks for conflicts, and returns a nice 'either' construct.
-- Either fails, or returns nothing.
checkLocalClash :: [(String,String)] -> Either FrontEndException ()
checkLocalClash ls =
    handleErrList localNameError (checkLocalClashH ls)


-- takes a list of modules and local names, and checks for conflicts in the local names.
-- The output is (file,file,name)
checkLocalClashH :: [(String,String)] -> [(String,String,String)]
checkLocalClashH [] = []
checkLocalClashH (pair:pairs) = 
    (checkOneClash pair pairs) ++ (checkLocalClashH pairs)


-- takes a single item and checks for clashes with the remaining items
checkOneClash :: (String,String) -> [(String,String)] -> [(String,String,String)]
checkOneClash _ [] = []
checkOneClash (a,na) ((b,nb):bs)
    | na == nb  = (a,b,na):(checkOneClash (a,na) bs)
    | otherwise = checkOneClash (a,na) bs


-----------------------------------------------------------------
-----------------------------------------------------------------
-- Part 5: checking for alias overwrites
-----------------------------------------------------------------
-----------------------------------------------------------------


-- Takes an 'allDefs' object, generated by a previous stage,
-- and returns either a conflicts error message or nothing.
-- takes (function aliases, process aliases, function definitions, process definitions)
checkForAliasClash :: AllDefs -> Either FrontEndException ()
checkForAliasClash (fas, pas, fds, pds) = do
    -- function alias-alias clashes
    handleErrList (aliasClashError "Function") (checkForAAClash fas)
    -- process alias-alias clashes
    handleErrList (aliasClashError "Process") (checkForAAClash pas)
    -- function alias-definition clashes
    handleErrList (aliasDefClashError "Function") (checkForADClash fas fds)
    -- process alias-definition clashes
    handleErrList (aliasDefClashError "Process") (checkForADClash pas pds)
    return ()


-- Checks for aliases which conflict with each other.
-- ('conflict' means they have the same object name.)
-- takes a list of (objName,modName,lineNum)
-- returns a list of (objName,modName1,line1,modName2,line2)
checkForAAClash :: [(String,String,Int)] -> [(String,String,Int,String,Int)]
checkForAAClash [] = []
checkForAAClash (s:ss) =
    (checkOneAAClash s ss) ++ (checkForAAClash ss)

-- Checks a single alias against a list of aliases
checkOneAAClash :: (String,String,Int) -> [(String,String,Int)] -> [(String,String,Int,String,Int)]
checkOneAAClash _ [] = []
checkOneAAClash (obj1,mod1,line1) ((obj2,mod2,line2):rest)
    | obj1 == obj2 =
        (obj1,mod1,line1,mod2,line2):(checkOneAAClash (obj1,mod1,line1) rest)
    | otherwise    = checkOneAAClash (obj1,mod1,line1) rest


-- Checks for conflicts between aliases and definitions.
-- Takes:   (objName,modName,lineNum) and (objName,lineNum)
-- Returns: (objName,modName,lineNum,lineNum)
checkForADClash :: [(String,String,Int)] -> [(String,Int)] -> [(String,String,Int,Int)]
checkForADClash [] _ = []
checkForADClash (s:ss) ts =
    (checkOneADClash s ts) ++ (checkForADClash ss ts)


checkOneADClash :: (String,String,Int) -> [(String,Int)] -> [(String,String,Int,Int)]
checkOneADClash _ [] = []
checkOneADClash (obj1,mod1,line1) ((obj2,line2):rest)
    | obj1 == obj2 = (obj1,mod1,line1,line2) : (checkOneADClash (obj1,mod1,line1) rest)
    | otherwise    = checkOneADClash (obj1,mod1,line1) rest


-----------------------------------------------------------------
-----------------------------------------------------------------
-- Part 6: Applying aliases
-----------------------------------------------------------------
-----------------------------------------------------------------


-- TODO de-aliasing imports


-- Takes an AST and alias list and de-aliases it.
-- Takes function substitutions, process substitutions, the AST to modify.
deAliasAll :: AllDefs -> B.MplProg -> B.MplProg
deAliasAll (funcSubs,procSubs,_,_) ast =
    astRefactor (deAliasOne funcSubs) Right (deAliasOne procSubs) Right ast



-- takes a list of module aliasing substitutions and applies it to a single input.
-- (Used by the 'renamer' module, so its return type is UFunc)
deAliasOne :: [(String,String,Int)] -> (String -> Either String (String,String))
deAliasOne [] a = Left a
deAliasOne ((objName,modName,_):rest) obj2
    | objName == obj2 = Right (modName,objName)
    | otherwise       = deAliasOne rest obj2