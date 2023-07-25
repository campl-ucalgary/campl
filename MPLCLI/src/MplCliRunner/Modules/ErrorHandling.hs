
module MplCliRunner.Modules.ErrorHandling (
    handleErrList,
    errorInFile,
    moduleInsideWhere,
    localNameError,
    aliasClashError,
    aliasDefClashError,
    cycleFound,
    cyclePrepend,
    cycleBase
    ) where

-- for the FrontEndException constructor 'ModuleException String'
import MplCliRunner.Stack

-- A module for generating error messages and handling possible errors


handleErrList :: (a -> String) -> [a] -> Either FrontEndException ()
handleErrList _ [] = Right ()
handleErrList f ns = Left $ ModuleException $ listErrors f ns


-- Takes a list of errors,
-- a function turning an error into a message,
-- and returns the concatenated messages of all the errors.
listErrors :: (a -> String) -> [a] -> String
listErrors _ [] = ""
listErrors f (b:bs) = (f b) ++ ('\n':'\n':(listErrors f bs))


-- Prepends 'error in file' to an error message.
-- Takes file, error
errorInFile :: String -> String -> String
errorInFile file err = concat
    ["Error in file: '", file, "'\n", err]


-- Detect imports inside 'where' block
moduleInsideWhere :: Int -> Either FrontEndException ()
moduleInsideWhere 0 = Right ()
moduleInsideWhere n = Left $ ModuleException
    ("error on line " ++ (show n) ++ ":\n\timports cannot be inside 'defn' blocks!")




-- the actual local name error
-- input: (file,file,name)
localNameError :: (String,String,String) -> String
localNameError (file1,file2,name) =
    concat ["Two modules have the same local name: '", name, "'\n\t",
        "'", file1, "'\n\t",
        "'", file2]


-- The error for when aliases clash with each other.
-- Takes a string with the object type (Function or Process)
-- Takes (objName,modName1,line1,modName2,line2)
aliasClashError :: String -> (String,String,Int,String,Int) -> String
aliasClashError objType (objName,mod1,line1,mod2,line2) = concat
    [objType, " '", objName, "' is ambiguous, referring either to:\n\t'", 
    mod1, ".", objName, "' (imported on line ", show line1, "), or\n\t'",
    mod2, ".", objName, "' (imported on line ", show line2, ")"]


-- Takes object_type, (objName,modName,lineNum,lineNum)
aliasDefClashError :: String -> (String,String,Int,Int) -> String
aliasDefClashError objType (objName,modName,line1,line2) = concat
    [objType, " '", objName, "' is ambiguous, referring either to:\n\t'", 
    modName, ".", objName, "' (imported on line ", show line1, "), or\n\t'",
    objName, "' (declared on line ", show line2, ")"]

-- Takes a cycle error message and adds a line at the start saying that a cycle was found.
cycleFound :: String -> String
cycleFound str =
    "Module cycle detected:\n" ++ str


-- Adds a new entry in a list of files that are part of a cycle.
cyclePrepend :: String -> String -> String
cyclePrepend file rest =
    concat ["\t'", file, "' ->\n", rest]


-- Takes a file and turns it into the last line of a cycle error message.
cycleBase :: String -> String
cycleBase str =
    concat ["\t'", str, "'"]