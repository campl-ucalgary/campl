{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module MplPasses.Renamer.RenameErrors where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed

import MplUtil.UniqueSupply

import MplPasses.Renamer.RenameSym
import MplPasses.PassesErrorsPprint

import Data.Foldable
import Data.Function
import Data.List

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Bool

data RenameErrors =
    OverlappingDeclarations [IdentP]
    | OutOfScope IdentP

  deriving Show

$(makeClassyPrisms ''RenameErrors)

class OverlappingDeclarations t where
    overlappingDeclarations :: AsRenameErrors e => t -> [e]

instance Foldable t => OverlappingDeclarations (t IdentP) where
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

instance OverlappingDeclarations (MplTypeClauseSpine MplParsed (SeqObjTag t)) where
    overlappingDeclarations (UMplTypeClauseSpine spine) = concatMap f allargs
      where
        f args = overlappingDeclarations (args <> NE.toList allstatevars)

        allargs = fmap (view typeClauseArgs) spine 
        allstatevars = fmap (view typeClauseStateVar) spine 

instance OverlappingDeclarations (MplTypeClauseSpine MplParsed (ConcObjTag t)) where
    overlappingDeclarations (UMplTypeClauseSpine spine) = concatMap f allargs
      where
        f args = overlappingDeclarations (uncurry mappend args <> NE.toList allstatevars)

        allargs :: NonEmpty ([IdentP], [IdentP])
        allargs = fmap (view typeClauseArgs) spine 

        allstatevars :: NonEmpty IdentP
        allstatevars = fmap (view typeClauseStateVar) spine 

-- default out of scope lookup
outOfScopeWith :: 
    AsRenameErrors a =>
     (IdentP -> t -> Maybe b) -> 
     t -> 
     IdentP -> 
     [a]
outOfScopeWith f symtab identp = 
    maybe [_OutOfScope # identp] (const []) 
        (f identp symtab)

outOfScopesWith :: 
    (Foldable t1, AsRenameErrors a) =>
     (IdentP -> t2 -> Maybe b) -> 
     t2 -> 
     t1 IdentP -> [a]
outOfScopesWith f symtab = 
    foldMap (outOfScopeWith f symtab)

pprintRenameErrors :: RenameErrors -> MplDoc
pprintRenameErrors = go
  where
    go :: RenameErrors -> MplDoc
    go = \case
        OverlappingDeclarations identps -> hsep
            [ pretty "Overlapping declarations with"
            , hsep $ map pprintIdentPWithLoc identps
            ]
        OutOfScope identp -> hsep
            [ pretty "Out of scope identifier"
            , pprintIdentPWithLoc identp
            ]
