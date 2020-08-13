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
module MplAST.MplParsed where

import Optics
import Optics.State.Operators
import Data.Void

import Data.Function

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE 

import MplAST.MplExpr
import MplAST.MplPattern
import MplAST.MplType
import MplAST.MplCmd
import MplAST.MplIdent
import MplAST.MplProg
import MplAST.MplExt

data IdentP = IdentP { 
        _identPNameOcc :: NameOcc
        , _identPNamespace :: Namespace
    }
  deriving Show
$(makeClassy ''IdentP)
$(makePrisms ''IdentP)

instance HasName IdentP where
    name = identPNameOcc % name

instance HasLocation IdentP where
    location = identPNameOcc % location

instance HasNamespace IdentP where
    namespace = identPNamespace

-- equality depends on the names and the 
-- namespaces only
instance Eq IdentP where
    IdentP n0 ns0 == IdentP n1 ns1 = n0 == n1 && ns0 == ns1

-- The type for MPL types..



type instance IdP MplParsed = IdentP
type instance ChP MplParsed = IdentP
type instance TypeP MplParsed = IdentP


-- Expression instances
type instance XMplExpr MplParsed = MplExpr MplParsed
type instance XEPOps MplParsed = ()
type instance XEVar MplParsed = ()
type instance XEInt MplParsed = Location
type instance XEChar MplParsed = Location
type instance XEDouble MplParsed = Location
type instance XECase MplParsed = ()
type instance XEObjCall MplParsed = ()
type instance XECall MplParsed = ()
type instance XERecord MplParsed = Location
type instance XERecordPhrase MplParsed = ()
type instance XXExpr MplParsed = Void
-- built in expression types
type instance XEList MplParsed = Location
type instance XEString MplParsed = Location
type instance XEUnit MplParsed = Location
type instance XETuple MplParsed = Location
type instance XEBuiltInOp MplParsed = Location
-- built in expression control
type instance XEIf MplParsed = ()
type instance XELet MplParsed = ()
type instance XEFold MplParsed = ()
type instance XEFoldPhrase MplParsed = ()
type instance XEUnfold MplParsed = ()
type instance XEUnfoldPhrase MplParsed = ()
type instance XESwitch MplParsed = ()

-- Pattern instances..
type instance XMplPattern MplParsed = MplPattern MplParsed
type instance XPConstructor MplParsed = ()
type instance XPRecord MplParsed = Location
type instance XPRecordPhrase MplParsed = ()
type instance XPVar MplParsed = ()
type instance XPNull MplParsed = Location
type instance XXPattern MplParsed = Void
-- built in..
type instance XPUnit MplParsed = Location
type instance XPTuple MplParsed = Location
type instance XPString MplParsed = Location
type instance XPInt MplParsed = Location
type instance XPChar MplParsed = Location
type instance XPList MplParsed = Location
type instance XPListCons MplParsed = Location

-- Process Command
type instance XMplCmd MplParsed = MplCmd MplParsed
type instance XCRun MplParsed = ()
type instance XCClose MplParsed = KeyWordNameOcc
type instance XCHalt MplParsed = KeyWordNameOcc
type instance XCGet MplParsed = KeyWordNameOcc
type instance XCPut MplParsed = KeyWordNameOcc
type instance XCHCase MplParsed = KeyWordNameOcc
type instance XCHPut MplParsed = KeyWordNameOcc
type instance XCSplit MplParsed = KeyWordNameOcc
type instance XCFork MplParsed = KeyWordNameOcc
type instance XCId MplParsed = KeyWordNameOcc
type instance XCIdNeg MplParsed = KeyWordNameOcc
type instance XCRace MplParsed = KeyWordNameOcc
type instance XCPlug MplParsed = Void
type instance XCPlugs MplParsed = 
    (KeyWordNameOcc, Maybe [(ChP MplParsed)])
        -- user can explictly write the channels 
        -- being bound. Note: due to a limitation of bnfc
        -- we can never actually write this
type instance XCCase MplParsed = KeyWordNameOcc
type instance XCSwitch MplParsed = KeyWordNameOcc
type instance XCHCasePhrase MplParsed  = ()
type instance XCForkPhrase MplParsed  = Maybe [(ChP MplParsed)] -- user can supply their own context
type instance XCPlugPhrase MplParsed  = Maybe [ChP MplParsed] -- user can supply their own context
type instance XXCmd MplParsed = Void

-- Type clause
type instance XTypeClauseSpineExt MplParsed = ()
type instance XTypeClauseExt MplParsed = ()
type instance XTypePhraseExt MplParsed  = ()

-- type phrases
type instance XTypePhraseFrom MplParsed SDataDefn = [XMplType MplParsed]
type instance XTypePhraseTo MplParsed SDataDefn = IdP MplParsed

type instance XTypePhraseFrom MplParsed SCodataDefn = 
    ([XMplType MplParsed], IdP MplParsed)
        -- args ++ [statevar]
        -- State var must be the last variable
type instance XTypePhraseTo MplParsed SCodataDefn = XMplType MplParsed

type instance XTypePhraseFrom MplParsed SProtocolDefn = XMplType MplParsed
type instance XTypePhraseTo MplParsed SProtocolDefn = IdP MplParsed

type instance XTypePhraseFrom MplParsed SCoprotocolDefn = IdP MplParsed
type instance XTypePhraseTo MplParsed SCoprotocolDefn = XMplType MplParsed

-- Function / process type
type instance XFunType MplParsed  = Maybe ([XMplType MplParsed], XMplType MplParsed)
type instance XProcType MplParsed = 
    Maybe ([XMplType MplParsed], [XMplType MplParsed], [XMplType MplParsed])

type instance XMplType MplParsed = MplType MplParsed
type instance XTypeWithArgs MplParsed = ()
type instance XTypeVar MplParsed = Void
type instance XXType MplParsed = Void
type instance XTypeIntF MplParsed = Location
type instance XTypeCharF MplParsed = Location
type instance XTypeDoubleF MplParsed = Location
type instance XTypeStringF MplParsed = Location
type instance XTypeUnitF MplParsed = Location
type instance XTypeBoolF MplParsed = Location
type instance XTypeListF MplParsed = Location
type instance XTypeTupleF MplParsed = Location

type instance XTypeGet MplParsed = Location
type instance XTypePut MplParsed = Location
type instance XTypeTensor MplParsed = Location
type instance XTypePar MplParsed = Location
type instance XTypeTopBot MplParsed = Location
type instance XTypeNeg MplParsed = Location
type instance XTypeSeqArrF MplParsed = Void
type instance XTypeConcArrF MplParsed = Void

