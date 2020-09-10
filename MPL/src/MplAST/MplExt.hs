{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module MplAST.MplExt where

{- Module for data types regarding which pass the compiler has completed
 - for the AST. We have 3 passes which occur in the following order:
 -   - parsing 
 -   - renaming
 -   - typechecking
 - Parsing is done completely by BNFC. Some ambiguities / extra checks are resolved 
 - after the parse phrase.
 -
 - Renaming resolves each symbol name to the appropriate declaration by giving them
 - the same unique identifier. Note this does not do any semantic checking..
 -
 - Type checking infers the type of the entire program. Moreover, it does a great
 - deal of semantic checking (i.e., checking if the concurrent instruction fork has disjoint channels)
 -}

data MplPass (c :: Ext)

type MplParsed = MplPass 'Parsed
type MplRenamed = MplPass 'Renamed
type MplTypeChecked = MplPass 'TypeChecked

data Ext =
    -- | After bnfc parsing
    Parsed
    -- | Giving each identifier
    -- a unique name
    | Renamed
    -- | Type checking
    | TypeChecked

