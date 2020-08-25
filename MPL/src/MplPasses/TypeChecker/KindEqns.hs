{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module MplPasses.TypeChecker.KindEqns where

import Optics 
import Optics.State.Operators

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed

import MplPasses.Env

data KindEqns = 
    KindExist [TypeP MplTypeChecked]
    | KindSubs [(KindSubTag, (TypeP MplTypeChecked, MplKind MplTypeChecked))]

data KindSubTag = KindPlainSubTag | KindStableSubTag 

$(concat <$> traverse makePrisms 
    [ ''KindEqns 
    , ''KindSubTag ]
 )



