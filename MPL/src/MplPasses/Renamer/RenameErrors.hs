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
import qualified Data.Set as Set

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Bool

data RenameErrors =
    OverlappingDeclarations [IdentP]
    | OverlappingDeclarationWithServices [IdentP]
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
            -- groupBy 
            --     (\a b -> a ^. name == b ^. name 
            --         && a ^. namespace == b ^. namespace) 
            --     idents'
            groupBy (\a b -> a ^. name == b ^. name && a ^. namespace == b ^. namespace)
                -- add a sort so that the groupBy will actually group any duplicates regardless of where they appear in the list
                $ sortOn (\i -> (i ^. name, i ^. namespace))
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


-- The names of handles that belong to services.
service_handle_names :: Set.Set String
service_handle_names = Set.fromList [
  "StringTerminalGet", 
  "StringTerminalPut", 
  "StringTerminalClose",
  "IntTerminalGet", 
  "IntTerminalPut", 
  "IntTerminalClose",
  "CharTerminalGet", 
  "CharTerminalPut", 
  "CharTerminalClose",  

  "ConsoleGet", 
  "ConsolePut", 
  "ConsoleClose",
  "IntConsoleGet", 
  "IntConsolePut", 
  "IntConsoleClose",
  "CharConsoleGet", 
  "CharConsolePut", 
  "CharConsoleClose",
  "ConsoleStringTerminal",

  "Timer",
  "TimerClose"
  ]

-- The names of service types.
service_type_names :: Set.Set String
service_type_names = Set.fromList [
  "Terminal",
  "Console",
  "Timer"
  ]

isServiceName :: (AsRenameErrors e) => e -> Bool
isServiceName errors =
  case errors ^? _OverlappingDeclarations of
    -- Nothing -> False
    -- Just [] -> False
    Just (dec:decs) ->
      -- if it's true that the name is the same as a service name
      -- in the same name space
      -- then we want to partition it out
      let dec_name = ((dec ^. name) ^. nameStr)
          dec_namespace = (dec ^. namespace)          
          isServiceType = (dec_name `Set.member` service_type_names)
            && (dec_namespace == TypeLevel)
          isServiceHandle = (dec_name `Set.member` service_handle_names)
            && (dec_namespace == ChannelLevel)
      in
        isServiceType || isServiceHandle
    _ -> False

updateErrorType :: (AsRenameErrors e) => e -> e
updateErrorType errors =
  case errors ^? _OverlappingDeclarations of
    Just (service_dec:decs) -> 
      if length decs <= 1
        then _OverlappingDeclarationWithServices # decs
        else _OverlappingDeclarations # decs
    _ -> errors


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
        OverlappingDeclarationWithServices identps -> hsep
            [ pretty "User-defined services have been depreceated. Remove overlapping declarations with services"
            , hsep $ map pprintIdentPWithLoc identps
            ]
        OutOfScope identp -> hsep
            [ pretty "Out of scope identifier"
            , pprintIdentPWithLoc identp
            ]


-- | The names of handles that belong to the Terminal service.
terminalHandles :: Set.Set String
terminalHandles = Set.fromList [
    "StringTerminalGet", 
    "StringTerminalPut", 
    "StringTerminalClose",
    "IntTerminalGet", 
    "IntTerminalPut", 
    "IntTerminalClose",
    "CharTerminalGet", 
    "CharTerminalPut", 
    "CharTerminalClose"
    ]

-- | The names of handles that belong to the Console service.
consoleHandles :: Set.Set String
consoleHandles = Set.fromList [
    "ConsoleGet", 
    "ConsolePut", 
    "ConsoleClose",
    "IntConsoleGet", 
    "IntConsolePut", 
    "IntConsoleClose",
    "CharConsoleGet", 
    "CharConsolePut", 
    "CharConsoleClose",
    "ConsoleStringTerminal"
    ]

-- | The names of handles that belong to the Timer service.
timerHandles :: Set.Set String
timerHandles = Set.fromList [
    "Timer",
    "TimerClose"
    ]
