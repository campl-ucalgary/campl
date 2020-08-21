{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MplPasses.TypeChecker.KindCheck where

import Optics 
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked 

import MplPasses.TypeChecker.TypeCheckErrors 
import MplPasses.TypeChecker.TypeCheckUtils 
import MplPasses.TypeChecker.TypeCheckSym 
import MplPasses.Env

kindCheckProcessType :: 
    TypeCheck 
        ([IdentR], [MplType MplRenamed], [MplType MplRenamed], [MplType MplRenamed]) 
        ([IdentT], [MplType MplTypeChecked], [MplType MplTypeChecked], [MplType MplTypeChecked])
kindCheckProcessType = undefined


