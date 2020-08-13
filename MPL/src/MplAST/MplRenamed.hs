{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module MplAST.MplRenamed where

import Optics
import Optics.State.Operators
import Data.Void

import Data.Function

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE 

import MplAST.MplParsed
import MplAST.MplExpr
import MplAST.MplPattern
import MplAST.MplType
import MplAST.MplCmd
import MplAST.MplIdent
import MplAST.MplProg
import MplAST.MplExt
import MplUtil.UniqueSupply 


data IdentR = IdentR { 
    _identRIdentP :: IdentP
    , _identRUniqueTag :: UniqueTag
 }
  deriving Show

$(makeClassy ''IdentR)
$(makePrisms ''IdentR)

instance HasUniqueTag IdentR where
    uniqueTag = identRUniqueTag 

instance HasIdentP IdentR where
    identP = identRIdentP 

instance HasName IdentR where
    name = identP % name

instance HasLocation IdentR where
    location = identP % location

instance HasNamespace IdentR where
    namespace = identP % namespace

data ChIdentR = ChIdentR {
    _chIdentRIdentR :: IdentR
    , _chIdentRPolarity :: Polarity
}

$(makeClassy ''ChIdentR)
$(makePrisms ''ChIdentR)

instance HasIdentR ChIdentR where
    identR = chIdentRIdentR

instance HasPolarity ChIdentR where
    polarity = chIdentRPolarity

instance HasUniqueTag ChIdentR where
    uniqueTag =  identR % uniqueTag

instance HasName ChIdentR where
    name = identR % name

instance HasLocation ChIdentR where
    location = identR % location

instance HasNamespace ChIdentR where
    namespace = identR % namespace


type instance IdP MplRenamed = IdentR
type instance ChP MplRenamed = ChIdentR
type instance TypeP MplRenamed = IdentR



-- Expression instances
type instance XMplExpr MplRenamed = MplExpr MplRenamed
type instance XEPOps MplRenamed = ()
type instance XEVar MplRenamed = ()
type instance XEInt MplRenamed = Location
type instance XEChar MplRenamed = Location
type instance XEDouble MplRenamed = Location
type instance XECase MplRenamed = ()
type instance XECall MplRenamed = ()
type instance XEObjCall MplRenamed = ()
type instance XERecord MplRenamed = Location
type instance XERecordPhrase MplRenamed = ()
type instance XXExpr MplRenamed = Void
-- built in expression types
type instance XEList MplRenamed = Location
type instance XEString MplRenamed = Location
type instance XEUnit MplRenamed = Location
type instance XETuple MplRenamed = Location
type instance XEBuiltInOp MplRenamed = Location
-- built in expression control
type instance XEIf MplRenamed = ()
type instance XELet MplRenamed = ()
type instance XEFold MplRenamed = ()
type instance XEFoldPhrase MplRenamed = ()
type instance XEUnfold MplRenamed = ()
type instance XEUnfoldPhrase MplRenamed = ()
type instance XESwitch MplRenamed = ()

-- Pattern instances..
type instance XMplPattern MplRenamed = MplPattern MplRenamed
type instance XPConstructor MplRenamed = ()
type instance XPRecord MplRenamed = Location
type instance XPRecordPhrase MplRenamed = ()
type instance XPVar MplRenamed = ()
type instance XPNull MplRenamed = Location
type instance XXPattern MplRenamed = Void
-- built in..
type instance XPUnit MplRenamed = Location
type instance XPTuple MplRenamed = Location
type instance XPString MplRenamed = Location
type instance XPInt MplRenamed = Location
type instance XPChar MplRenamed = Location
type instance XPList MplRenamed = Location
type instance XPListCons MplRenamed = Location

-- Process Command
type instance XMplCmd MplRenamed = MplCmd MplRenamed
type instance XCRun MplRenamed = ()
type instance XCClose MplRenamed = KeyWordNameOcc
type instance XCHalt MplRenamed = KeyWordNameOcc
type instance XCGet MplRenamed = KeyWordNameOcc
type instance XCPut MplRenamed = KeyWordNameOcc
type instance XCHCase MplRenamed = KeyWordNameOcc
type instance XCHPut MplRenamed = KeyWordNameOcc
type instance XCSplit MplRenamed = KeyWordNameOcc
type instance XCFork MplRenamed = KeyWordNameOcc
type instance XCId MplRenamed = KeyWordNameOcc
type instance XCIdNeg MplRenamed = KeyWordNameOcc
type instance XCRace MplRenamed = KeyWordNameOcc
type instance XCPlug MplRenamed = Void
type instance XCPlugs MplRenamed = KeyWordNameOcc
type instance XCCase MplRenamed = KeyWordNameOcc
type instance XCSwitch MplRenamed = KeyWordNameOcc
type instance XCHCasePhrase MplRenamed  = ()
type instance XCForkPhrase MplRenamed  = Maybe [(ChP MplRenamed)] -- user can supply their own context
type instance XCPlugPhrase MplRenamed  = Maybe [ChP MplRenamed] -- user can supply their own context
type instance XXCmd MplRenamed = Void

-- Type clause
type instance XTypeClauseSpineExt MplRenamed = ()
type instance XTypeClauseExt MplRenamed = ()
type instance XTypePhraseExt MplRenamed  = ()

type instance XTypePhraseFrom MplRenamed SDataDefn = 
    [XMplType MplRenamed]
type instance XTypePhraseTo MplRenamed SDataDefn = 
    XMplType MplRenamed

type instance XTypePhraseTo MplRenamed t = 
    XMplType MplRenamed

type instance XTypePhraseFrom MplRenamed SDataDefn = 
    [XMplType MplRenamed]
type instance XTypePhraseFrom MplRenamed SCodataDefn = 
    ([XMplType MplRenamed], XMplType MplRenamed)
        -- args ++ [statevar]
        -- State var must be the last variable
type instance XTypePhraseFrom MplRenamed SProtocolDefn = 
    XMplType MplRenamed
type instance XTypePhraseFrom MplRenamed SCoprotocolDefn = 
    XMplType MplRenamed

-- Function / process type
type instance XFunType MplRenamed  = Maybe ([XMplType MplRenamed], XMplType MplRenamed)
type instance XProcType MplRenamed = 
    Maybe ([XMplType MplRenamed], [XMplType MplRenamed], [XMplType MplRenamed])

type instance XMplType MplRenamed = MplType MplRenamed
type instance XTypeWithArgs MplRenamed = ()
type instance XTypeVar MplRenamed = ()
type instance XXType MplRenamed = Void
type instance XTypeIntF MplRenamed = Location
type instance XTypeCharF MplRenamed = Location
type instance XTypeDoubleF MplRenamed = Location
type instance XTypeStringF MplRenamed = Location
type instance XTypeUnitF MplRenamed = Location
type instance XTypeBoolF MplParsed = Location
type instance XTypeListF MplRenamed = Location
type instance XTypeTupleF MplRenamed = Location

type instance XTypeGet MplRenamed = Location
type instance XTypePut MplRenamed = Location
type instance XTypeTensor MplRenamed = Location
type instance XTypePar MplRenamed = Location
type instance XTypeTopBot MplRenamed = Location
type instance XTypeNeg MplRenamed = Location
