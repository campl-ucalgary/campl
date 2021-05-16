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
module MplAST.MplPatternCompiled where

import Optics
import Optics.State.Operators
import Data.Void

import Data.Function

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE 

import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import MplAST.MplExpr
import MplAST.MplPattern
import MplAST.MplType
import MplAST.MplCmd
import MplAST.MplIdent
import MplAST.MplProg
import MplAST.MplExt
import MplAST.MplKind 
import MplUtil.UniqueSupply 

{- Module for defining the AST after compilation of pattern matching -}

type instance IdP MplPatternCompiled = IdP MplTypeChecked 
type instance ChP MplPatternCompiled = ChP MplTypeChecked 
type instance TypeP MplPatternCompiled = TypeP MplTypeChecked 

type instance XMplType MplPatternCompiled = MplType MplTypeChecked 

-- definitions.
type instance XDataDefn MplPatternCompiled  = 
    MplTypeClauseSpine MplPatternCompiled (SeqObjTag DataDefnTag)
type instance XCodataDefn MplPatternCompiled  = 
    MplTypeClauseSpine MplPatternCompiled (SeqObjTag CodataDefnTag)
type instance XProtocolDefn MplPatternCompiled  = 
    MplTypeClauseSpine MplPatternCompiled (ConcObjTag ProtocolDefnTag)
type instance XCoprotocolDefn MplPatternCompiled  = 
    MplTypeClauseSpine MplPatternCompiled (ConcObjTag CoprotocolDefnTag)

type instance XFunctionDefn MplPatternCompiled = MplFunction MplPatternCompiled
type instance XProcessDefn MplPatternCompiled  = MplProcess MplPatternCompiled 

-- Expression instances
type instance XMplExpr MplPatternCompiled = MplExpr MplPatternCompiled
type instance XEPOps MplPatternCompiled = XEPOps MplTypeChecked
type instance XEVar MplPatternCompiled = XEVar MplTypeChecked

type instance XEInt MplPatternCompiled = MplSeqType MplTypeChecked
type instance XEChar MplPatternCompiled = MplSeqType MplTypeChecked
type instance XEDouble MplPatternCompiled = MplSeqType MplTypeChecked
type instance XECase MplPatternCompiled = MplSeqType MplTypeChecked
type instance XECasePattern MplPatternCompiled =  XECasePattern MplTypeChecked
type instance XECall MplPatternCompiled = XECall MplTypeChecked

type instance XEObjCall MplPatternCompiled = XEObjCall MplTypeChecked
type instance XERecord MplPatternCompiled = MplSeqType MplPatternCompiled
type instance XERecordPhrase MplPatternCompiled = MplTypePhrase MplPatternCompiled (SeqObjTag CodataDefnTag)
type instance XXExpr MplPatternCompiled = Void

-- built in expression types
type instance XEList MplPatternCompiled = (MplSeqType MplTypeChecked)
type instance XEString MplPatternCompiled = (MplSeqType MplTypeChecked)
type instance XEUnit MplPatternCompiled = (MplSeqType MplTypeChecked)
type instance XETuple MplPatternCompiled = (MplSeqType MplTypeChecked)
type instance XEBuiltInOp MplPatternCompiled = (MplSeqType MplTypeChecked)

-- built in expression control
type instance XEIf MplPatternCompiled = (MplSeqType MplTypeChecked)
type instance XELet MplPatternCompiled = ()
type instance XEFold MplPatternCompiled = MplSeqType MplPatternCompiled
type instance XEFoldPhrase MplPatternCompiled = 
    -- (MplTypePhrase MplPatternCompiled (SeqObjTag DataDefnTag), MplSeqType MplPatternCompiled)
    MplTypePhrase MplPatternCompiled (SeqObjTag DataDefnTag)
type instance XEUnfold MplPatternCompiled = MplSeqType MplPatternCompiled
type instance XEUnfoldPhrase MplPatternCompiled = 
    ()
type instance XEUnfoldSubPhrase MplPatternCompiled = 
    MplTypePhrase MplPatternCompiled (SeqObjTag CodataDefnTag)
    -- (MplTypePhrase MplPatternCompiled (SeqObjTag CodataDefnTag), MplSeqType MplPatternCompiled)
-- type instance XESwitch MplPatternCompiled = MplSeqType MplPatternCompiled
type instance XESwitch MplPatternCompiled = Void

-- Pattern instances..
type instance XMplPattern MplPatternCompiled = MplPattern MplPatternCompiled
type instance XPConstructor MplPatternCompiled = Void
type instance XPRecord MplPatternCompiled = Void
type instance XPRecordPhrase MplPatternCompiled = Void

type instance XPVar MplPatternCompiled = MplSeqType MplPatternCompiled
type instance XPNull MplPatternCompiled = Void
type instance XXPattern MplPatternCompiled = Void

-- built in..
type instance XPUnit MplPatternCompiled = Void
type instance XPTuple MplPatternCompiled = Void
type instance XPString MplPatternCompiled = Void
type instance XPInt MplPatternCompiled = Void
type instance XPChar MplPatternCompiled = Void
type instance XPList MplPatternCompiled = Void
type instance XPListCons MplPatternCompiled =Void

-- Process Command
type instance XMplCmd MplPatternCompiled = MplCmd MplPatternCompiled
type instance XCRun MplPatternCompiled = MplProcess MplPatternCompiled
type instance XCClose MplPatternCompiled = KeyWordNameOcc
type instance XCHalt MplPatternCompiled = KeyWordNameOcc
type instance XCGet MplPatternCompiled = KeyWordNameOcc
type instance XCPut MplPatternCompiled = KeyWordNameOcc
type instance XCHCase MplPatternCompiled = KeyWordNameOcc
type instance XCHPut MplPatternCompiled = (KeyWordNameOcc, MplConcObjDefn MplPatternCompiled)
type instance XCSplit MplPatternCompiled = KeyWordNameOcc
type instance XCFork MplPatternCompiled = KeyWordNameOcc
type instance XCId MplPatternCompiled = KeyWordNameOcc
type instance XCIdNeg MplPatternCompiled = KeyWordNameOcc
type instance XCRace MplPatternCompiled = KeyWordNameOcc
type instance XCPlug MplPatternCompiled = Void
type instance XCPlugs MplPatternCompiled = 
    ( KeyWordNameOcc
    , [(IdP MplPatternCompiled, ([TypeP MplPatternCompiled], XMplType MplPatternCompiled))])
                                                    -- these are the new plugged channels.
                                                    -- Note that these do not have a polarity 
                                                    -- because it changes based on the phrase
type instance XCCase MplPatternCompiled = KeyWordNameOcc
type instance XCSwitch MplPatternCompiled = KeyWordNameOcc
type instance XCHCasePhrase MplPatternCompiled  = MplConcObjDefn MplPatternCompiled
type instance XCForkPhrase MplPatternCompiled  = [ChP MplPatternCompiled] 
type instance XCPlugPhrase MplPatternCompiled  = ()
type instance XXCmd MplPatternCompiled = Void

type instance XTypeClauseSpineExt MplPatternCompiled t = ()
type instance XTypeClauseExt MplPatternCompiled t = MplTypeClauseSpine MplTypeChecked t
-- type instance XTypeClauseExt MplPatternCompiled t = ()
type instance XTypePhraseExt MplPatternCompiled t = MplTypeClause MplTypeChecked t
-- type instance XTypePhraseExt MplTypeChecked t = ()

type instance XTypePhraseTo MplPatternCompiled t = 
    XMplType MplTypeChecked
type instance XTypePhraseFrom MplPatternCompiled (SeqObjTag DataDefnTag) = 
    [XMplType MplTypeChecked]
type instance XTypePhraseFrom MplPatternCompiled (SeqObjTag CodataDefnTag) = 
    ([XMplType MplTypeChecked], XMplType MplTypeChecked) -- args ++ [statevar] State var must be the last variable
type instance XTypePhraseFrom MplPatternCompiled (ConcObjTag ProtocolDefnTag) = 
    XMplType MplTypeChecked
type instance XTypePhraseFrom MplPatternCompiled (ConcObjTag CoprotocolDefnTag) = 
    XMplType MplTypeChecked

-- Function / process type
type instance XFunType MplPatternCompiled  = 
    ([TypeP MplTypeChecked], [XMplType MplTypeChecked], XMplType MplTypeChecked)
type instance XProcType MplPatternCompiled = 
    ([TypeP MplPatternCompiled], [XMplType MplPatternCompiled], [XMplType MplPatternCompiled], [XMplType MplPatternCompiled])

type instance XTypeSeqWithArgs MplPatternCompiled = MplSeqObjDefn MplPatternCompiled
type instance XTypeSeqVarWithArgs MplPatternCompiled = Void -- higher kinded types are not allowed (for now)
type instance XTypeConcWithArgs MplPatternCompiled = 
    MplConcObjDefn MplPatternCompiled
type instance XTypeConcVarWithArgs  MplPatternCompiled = Void -- higher kinded types are not allowed (for now)

type instance XTypeVar MplPatternCompiled = 
    Maybe (XMplKind MplPatternCompiled)
type instance XTypeWithNoArgs MplPatternCompiled = 
    MplObjectDefn MplPatternCompiled
type instance XXType MplPatternCompiled = Void
type instance XTypeIntF MplPatternCompiled = Maybe NameOcc
type instance XTypeCharF MplPatternCompiled = Maybe NameOcc
type instance XTypeDoubleF MplPatternCompiled = Maybe NameOcc
type instance XTypeStringF MplPatternCompiled = Maybe NameOcc
type instance XTypeUnitF MplPatternCompiled = Maybe NameOcc
type instance XTypeBoolF MplPatternCompiled = Maybe NameOcc
type instance XTypeListF MplPatternCompiled = Maybe NameOcc
type instance XTypeTupleF MplPatternCompiled = Maybe NameOcc

type instance XTypeGet MplPatternCompiled = Maybe NameOcc
type instance XTypePut MplPatternCompiled = Maybe NameOcc
type instance XTypeTensor MplPatternCompiled = Maybe NameOcc
type instance XTypePar MplPatternCompiled = Maybe NameOcc
type instance XTypeTopBot MplPatternCompiled = Maybe NameOcc
type instance XTypeNeg MplPatternCompiled = Maybe NameOcc
type instance XTypeSeqArrF MplPatternCompiled = ()
type instance XTypeConcArrF MplPatternCompiled = ()

type instance XXMplBuiltInTypesF MplPatternCompiled = Void

-- Kind info..
-------------------------
type instance XMplKind MplPatternCompiled = MplPrimitiveKind MplPatternCompiled
type instance XSeqKind MplPatternCompiled = ()
type instance XConcKind MplPatternCompiled = ()
type instance XArrKind MplPatternCompiled = Void
type instance XSeqArgKind MplPatternCompiled = Void
type instance XConcArgKind MplPatternCompiled = Void
type instance XKindVar MplPatternCompiled = Void

type instance KindP MplPatternCompiled = Void

type instance XXKind MplPatternCompiled = Void

