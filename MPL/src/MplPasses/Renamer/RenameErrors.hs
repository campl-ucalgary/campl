{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
module MplPasses.Renamer.RenameErrors where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed

import MplUtil.UniqueSupply

import MplPasses.Renamer.RenameSym

import Data.Foldable
import Data.Function
import Data.List

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

data RenameError =
    OverlappingDeclarations [IdentP]
    | OutOfScope IdentP

    | TypeClauseGraphArgsMustContainTheSameTypeVariables 
        [NonEmpty [IdentP]]
        -- list of equivalence classes of the arguments on (==)
    | ExpectedStateVarButGot IdentP IdentP 
        -- expected, actual

    | IllegalLastCommand IdentP 
    | IllegalNonLastCommand IdentP 

    | InternalRenameError 

$(makeClassyPrisms ''RenameError)

overlappingDeclarations ::
    ( AsRenameError e
    , Foldable t ) =>
    t IdentP -> [e]
overlappingDeclarations idents = duplicates
  where
    idents' = toList idents
    identseqclasses = 
        groupBy 
            (\a b -> a ^. name == b ^. name 
                && a ^. namespace == b ^. namespace) 
            idents'

    duplicates = foldMap f identseqclasses
      where
        f lst | length lst >= 2 = [_OverlappingDeclarations # lst]
              | otherwise = []

-- default out of scope lookup
outOfScope :: 
    AsRenameError e =>
    SymTab -> 
    IdentP -> 
    [e]
outOfScope symtab identp = 
    maybe [_OutOfScope # identp] (const []) 
        (lookupSym (identp ^. name) _Nothing symtab)

outOfScopes ::
    ( Foldable t 
    , AsRenameError e) =>
    SymTab -> t IdentP -> [e]
outOfScopes symtab = foldMap (outOfScope symtab)

    
outOfScopeWith f symtab identp = 
    maybe [_OutOfScope # identp] (const []) 
        (f (identp ^. name) symtab)

outOfScopesWith f symtab identp = 
    foldMap (outOfScopeWith f symtab)
