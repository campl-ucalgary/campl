
module MplCliRunner.Modules.ErrorHandling (moduleInsideWhere) where

-- A module for generating error messages and handling possible errors




-- Detect imports inside 'where' block
moduleInsideWhere :: Int -> Either String ()
moduleInsideWhere 0 = ()
moduleInsideWhere n = "error on line " ++ (show n) ++ ": imports cannot be inside 'defn' blocks!"

