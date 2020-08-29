{-# LANGUAGE DeriveDataTypeable #-}
module MplPasses.TypeChecker.TypeCheckPanic where

import Data.Data

panicSymTab :: a
panicSymTab = error "Illegal symbol table lookup -- most likely a thunk was evaluated too early."

