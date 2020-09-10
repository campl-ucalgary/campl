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
module MplAST.MplTypeChecked where

import Optics
import Optics.State.Operators
import Data.Void

import Data.Function

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE 

import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplExpr
import MplAST.MplPattern
import MplAST.MplType
import MplAST.MplCmd
import MplAST.MplIdent
import MplAST.MplProg
import MplAST.MplExt
import MplAST.MplKind 
import MplUtil.UniqueSupply 

{- Module for defining the AST after the AST has been type checked
  -}

type IdentT = IdentR

data TypeT = 
    NamedType IdentT
    | GenNamedType UniqueTag
  deriving Show

instance Eq TypeT where
    NamedType a == NamedType b = a ^. uniqueTag == b ^. uniqueTag
    GenNamedType a == GenNamedType b = a  == b 
    NamedType a == GenNamedType b = a ^. uniqueTag  == b 
    GenNamedType a == NamedType b = a == b ^. uniqueTag 

instance Ord TypeT where
    NamedType a <= NamedType b = a ^. uniqueTag <= b ^. uniqueTag
    GenNamedType a <= GenNamedType b = a <= b 

$(makePrisms ''TypeT)


data ChIdentT = ChIdentT {
    _chIdentTChIdentR :: ChIdentR
    , _chIdentTType :: MplChType MplTypeChecked 
}  

deriving instance Show (XMplType MplTypeChecked) => Show ChIdentT

$(makeClassy ''ChIdentT)
$(makePrisms ''ChIdentT)

instance Eq ChIdentT where
    a == b = a ^. chIdentR == b ^. chIdentR

instance HasChIdentR ChIdentT where
    chIdentR = chIdentTChIdentR 

instance HasIdentR ChIdentT where
    identR = chIdentTChIdentR % identR

instance HasPolarity ChIdentT where
    polarity = chIdentR % polarity

instance HasUniqueTag ChIdentT where
    uniqueTag = identR % uniqueTag

instance HasName ChIdentT where
    name = identR % name

instance HasLocation ChIdentT where
    location = identR % location

instance HasNamespace ChIdentT where
    namespace = identR % namespace

data ExprCallDef = 
    ExprCallPattern (MplPattern MplTypeChecked)
    | ExprCallFun (MplFunction MplTypeChecked)
  deriving Show




type instance IdP MplTypeChecked = IdentT
type instance ChP MplTypeChecked = ChIdentT
type instance TypeP MplTypeChecked = TypeT

type instance XMplType MplTypeChecked = MplType MplTypeChecked

-- definitions.
type instance XDataDefn MplTypeChecked  = 
    MplTypeClauseSpine MplTypeChecked (SeqObjTag DataDefnTag)
type instance XCodataDefn MplTypeChecked  = 
    MplTypeClauseSpine MplTypeChecked (SeqObjTag CodataDefnTag)
type instance XProtocolDefn MplTypeChecked  = 
    MplTypeClauseSpine MplTypeChecked (ConcObjTag ProtocolDefnTag)
type instance XCoprotocolDefn MplTypeChecked  = 
    MplTypeClauseSpine MplTypeChecked (ConcObjTag CoprotocolDefnTag)

type instance XFunctionDefn MplTypeChecked = MplFunction MplTypeChecked
type instance XProcessDefn MplTypeChecked  = MplProcess MplTypeChecked 

-- Expression instances
type instance XMplExpr MplTypeChecked = MplExpr MplTypeChecked
type instance XEPOps MplTypeChecked = MplSeqType MplTypeChecked
type instance XEVar MplTypeChecked = 
    (ExprCallDef , MplSeqType MplTypeChecked)
type instance XEInt MplTypeChecked = (Location, MplSeqType MplTypeChecked)
type instance XEChar MplTypeChecked = (Location, MplSeqType MplTypeChecked)
type instance XEDouble MplTypeChecked = (Location, MplSeqType MplTypeChecked)
type instance XECase MplTypeChecked = MplSeqType MplTypeChecked
type instance XECall MplTypeChecked = MplSeqType MplTypeChecked
type instance XEObjCall MplTypeChecked = MplSeqType MplTypeChecked
type instance XERecord MplTypeChecked = (Location, MplSeqType MplTypeChecked)
type instance XERecordPhrase MplTypeChecked = MplTypePhrase MplTypeChecked (SeqObjTag CodataDefnTag)
type instance XXExpr MplTypeChecked = Void

-- built in expression types
type instance XEList MplTypeChecked = (Location, MplSeqType MplTypeChecked)
type instance XEString MplTypeChecked = (Location, MplSeqType MplTypeChecked)
type instance XEUnit MplTypeChecked = (Location, MplSeqType MplTypeChecked)
type instance XETuple MplTypeChecked = (Location, MplSeqType MplTypeChecked)
type instance XEBuiltInOp MplTypeChecked = (Location, MplSeqType MplTypeChecked)
-- built in expression control
type instance XEIf MplTypeChecked = MplSeqType MplTypeChecked
type instance XELet MplTypeChecked = ()
type instance XEFold MplTypeChecked = MplSeqType MplTypeChecked
type instance XEFoldPhrase MplTypeChecked = 
    -- (MplTypePhrase MplTypeChecked (SeqObjTag DataDefnTag), MplSeqType MplTypeChecked)
    MplTypePhrase MplTypeChecked (SeqObjTag DataDefnTag)
type instance XEUnfold MplTypeChecked = MplSeqType MplTypeChecked
type instance XEUnfoldPhrase MplTypeChecked = 
    ()
type instance XEUnfoldSubPhrase MplTypeChecked = 
    MplTypePhrase MplTypeChecked (SeqObjTag CodataDefnTag)
    -- (MplTypePhrase MplTypeChecked (SeqObjTag CodataDefnTag), MplSeqType MplTypeChecked)
type instance XESwitch MplTypeChecked = MplSeqType MplTypeChecked

-- Pattern instances..
type instance XMplPattern MplTypeChecked = MplPattern MplTypeChecked
type instance XPConstructor MplTypeChecked = 
    (MplTypePhrase MplTypeChecked (SeqObjTag DataDefnTag), MplSeqType MplTypeChecked)
type instance XPRecord MplTypeChecked = 
    (Location, MplSeqType MplTypeChecked)
type instance XPRecordPhrase MplTypeChecked = 
    -- (MplTypePhrase MplTypeChecked (SeqObjTag CodataDefnTag), MplSeqType MplTypeChecked)
    MplTypePhrase MplTypeChecked (SeqObjTag CodataDefnTag)
type instance XPVar MplTypeChecked = MplSeqType MplTypeChecked
type instance XPNull MplTypeChecked = (Location, MplSeqType MplTypeChecked)
type instance XXPattern MplTypeChecked = Void
-- built in..
type instance XPUnit MplTypeChecked = (Location, MplSeqType MplTypeChecked)
type instance XPTuple MplTypeChecked = (Location, MplSeqType MplTypeChecked)
type instance XPString MplTypeChecked = (Location, MplSeqType MplTypeChecked)
type instance XPInt MplTypeChecked = (Location, MplSeqType MplTypeChecked)
type instance XPChar MplTypeChecked = (Location, MplSeqType MplTypeChecked)
type instance XPList MplTypeChecked = (Location, MplSeqType MplTypeChecked)
type instance XPListCons MplTypeChecked = (Location, MplSeqType MplTypeChecked)

-- Process Command
type instance XMplCmd MplTypeChecked = MplCmd MplTypeChecked
type instance XCRun MplTypeChecked = MplProcess MplTypeChecked
type instance XCClose MplTypeChecked = KeyWordNameOcc
type instance XCHalt MplTypeChecked = KeyWordNameOcc
type instance XCGet MplTypeChecked = KeyWordNameOcc
type instance XCPut MplTypeChecked = KeyWordNameOcc
type instance XCHCase MplTypeChecked = KeyWordNameOcc
type instance XCHPut MplTypeChecked = (KeyWordNameOcc, MplConcObjDefn MplTypeCheckedPhrase)
type instance XCSplit MplTypeChecked = KeyWordNameOcc
type instance XCFork MplTypeChecked = KeyWordNameOcc
type instance XCId MplTypeChecked = KeyWordNameOcc
type instance XCIdNeg MplTypeChecked = KeyWordNameOcc
type instance XCRace MplTypeChecked = KeyWordNameOcc
type instance XCPlug MplTypeChecked = Void
type instance XCPlugs MplTypeChecked = 
    ( KeyWordNameOcc
    , [(IdP MplTypeChecked, ([TypeP MplTypeChecked], XMplType MplTypeChecked))])
                                                    -- these are the new plugged channels.
                                                    -- Note that these do not have a polarity 
                                                    -- because it changes based on the phrase
type instance XCCase MplTypeChecked = KeyWordNameOcc
type instance XCSwitch MplTypeChecked = KeyWordNameOcc
type instance XCHCasePhrase MplTypeChecked  = MplConcObjDefn MplTypeCheckedPhrase
type instance XCForkPhrase MplTypeChecked  = [ChP MplTypeChecked] 
type instance XCPlugPhrase MplTypeChecked  = ()
type instance XXCmd MplTypeChecked = Void

type instance XTypeClauseSpineExt MplTypeChecked t = ()
type instance XTypeClauseExt MplTypeChecked t = MplTypeClauseSpine MplTypeChecked t
-- type instance XTypeClauseExt MplTypeChecked t = ()
type instance XTypePhraseExt MplTypeChecked t = MplTypeClause MplTypeChecked t
-- type instance XTypePhraseExt MplTypeChecked t = ()

type instance XTypePhraseTo MplTypeChecked t = 
    XMplType MplTypeChecked
type instance XTypePhraseFrom MplTypeChecked (SeqObjTag DataDefnTag) = 
    [XMplType MplTypeChecked]
type instance XTypePhraseFrom MplTypeChecked (SeqObjTag CodataDefnTag) = 
    ([XMplType MplTypeChecked], XMplType MplTypeChecked) -- args ++ [statevar] State var must be the last variable
type instance XTypePhraseFrom MplTypeChecked (ConcObjTag ProtocolDefnTag) = 
    XMplType MplTypeChecked
type instance XTypePhraseFrom MplTypeChecked (ConcObjTag CoprotocolDefnTag) = 
    XMplType MplTypeChecked

-- Function / process type
type instance XFunType MplTypeChecked  = ([TypeP MplTypeChecked], [XMplType MplTypeChecked], XMplType MplTypeChecked)
type instance XProcType MplTypeChecked = 
    ([TypeP MplTypeChecked], [XMplType MplTypeChecked], [XMplType MplTypeChecked], [XMplType MplTypeChecked])

type instance XMplType MplTypeChecked = MplType MplTypeChecked
type instance XTypeSeqWithArgs MplTypeChecked = MplSeqObjDefn MplTypeCheckedClause
type instance XTypeSeqVarWithArgs MplTypeChecked = Void -- higher kinded types are not allowed (for now)
type instance XTypeConcWithArgs MplTypeChecked = 
    MplConcObjDefn MplTypeCheckedClause
type instance XTypeConcVarWithArgs  MplTypeChecked = Void -- higher kinded types are not allowed (for now)

type instance XTypeVar MplTypeChecked = 
    Maybe (XMplKind MplTypeChecked)
type instance XTypeWithNoArgs MplTypeChecked = 
    MplObjectDefn MplTypeCheckedClause
type instance XXType MplTypeChecked = Void
type instance XTypeIntF MplTypeChecked = NameOcc
type instance XTypeCharF MplTypeChecked = NameOcc
type instance XTypeDoubleF MplTypeChecked = NameOcc
type instance XTypeStringF MplTypeChecked = NameOcc
type instance XTypeUnitF MplTypeChecked = NameOcc
type instance XTypeBoolF MplTypeChecked = NameOcc
type instance XTypeListF MplTypeChecked = NameOcc
type instance XTypeTupleF MplTypeChecked = NameOcc

type instance XTypeGet MplTypeChecked = Maybe NameOcc
type instance XTypePut MplTypeChecked = Maybe NameOcc
type instance XTypeTensor MplTypeChecked = Maybe NameOcc
type instance XTypePar MplTypeChecked = Maybe NameOcc
type instance XTypeTopBot MplTypeChecked = Maybe NameOcc
type instance XTypeNeg MplTypeChecked = Maybe NameOcc
type instance XTypeSeqArrF MplTypeChecked = ()
type instance XTypeConcArrF MplTypeChecked = ()

type instance XXMplBuiltInTypesF MplTypeChecked = Void


-- Kind info..
-------------------------
type instance XMplKind MplTypeChecked = MplPrimitiveKind MplTypeChecked
type instance XSeqKind MplTypeChecked = ()
type instance XConcKind MplTypeChecked = ()
type instance XArrKind MplTypeChecked = Void
type instance XSeqArgKind MplTypeChecked = Void
type instance XConcArgKind MplTypeChecked = Void
type instance XKindVar MplTypeChecked = Void

type instance KindP MplTypeChecked = Void

type instance XXKind MplTypeChecked = Void

-- Kind annotation for data
-------------------------
data MplTypeCheckedClause

type instance XDataDefn MplTypeCheckedClause  = 
    MplTypeClause MplTypeChecked (SeqObjTag DataDefnTag)
type instance XCodataDefn MplTypeCheckedClause  = 
    MplTypeClause MplTypeChecked (SeqObjTag CodataDefnTag)
type instance XProtocolDefn MplTypeCheckedClause  = 
    MplTypeClause MplTypeChecked (ConcObjTag ProtocolDefnTag)
type instance XCoprotocolDefn MplTypeCheckedClause  = 
    MplTypeClause MplTypeChecked (ConcObjTag CoprotocolDefnTag)

type instance XProcessDefn MplTypeCheckedClause  = 
    XProcessDefn MplTypeChecked
type instance XFunctionDefn MplTypeCheckedClause  = 
    XFunctionDefn MplTypeChecked

data MplTypeCheckedPhrase

type instance XDataDefn MplTypeCheckedPhrase  = 
    MplTypePhrase MplTypeChecked (SeqObjTag DataDefnTag)
type instance XCodataDefn MplTypeCheckedPhrase  = 
    MplTypePhrase MplTypeChecked (SeqObjTag CodataDefnTag)
type instance XProtocolDefn MplTypeCheckedPhrase  = 
    MplTypePhrase MplTypeChecked (ConcObjTag ProtocolDefnTag)
type instance XCoprotocolDefn MplTypeCheckedPhrase  = 
    MplTypePhrase MplTypeChecked (ConcObjTag CoprotocolDefnTag)

$(makePrisms ''ExprCallDef)
