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

data MplPatternCompiledCase

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

type instance XEInt MplPatternCompiled = XMplType MplTypeChecked
type instance XEChar MplPatternCompiled = XMplType MplTypeChecked
type instance XEBool MplPatternCompiled = XMplType MplTypeChecked
type instance XEDouble MplPatternCompiled = XMplType MplTypeChecked
type instance XECase MplPatternCompiled = XMplType MplTypeChecked
type instance XECasePattern MplPatternCompiled =  MplPattern MplPatternCompiledCase

---------------------------
-- Pattern in a case
---------------------------
type instance IdP MplPatternCompiledCase = IdP MplPatternCompiled 
type instance ChP MplPatternCompiledCase = ChP MplPatternCompiled 
type instance TypeP MplPatternCompiledCase = TypeP MplPatternCompiled 

type instance XMplPattern MplPatternCompiledCase = Void
type instance XPConstructor MplPatternCompiledCase = Void

type instance XPSimpleConstructor MplPatternCompiledCase = 
    XPConstructor MplTypeChecked
type instance XPSimpleConstructorArgs MplPatternCompiledCase = 
    [(IdP MplTypeChecked, XMplType MplTypeChecked)]
    -- (Variable name, Type)

type instance XPRecord MplPatternCompiledCase = Void
type instance XPRecordPhrase MplPatternCompiledCase = Void

type instance XPVar MplPatternCompiledCase = Void
type instance XPNull MplPatternCompiledCase = Void
type instance XXPattern MplPatternCompiledCase = Void

-- built in..
type instance XPUnit MplPatternCompiledCase = Void
type instance XPTuple MplPatternCompiledCase = Void
type instance XPString MplPatternCompiledCase = Void
type instance XPInt MplPatternCompiledCase = Void
type instance XPBool MplPatternCompiledCase = Void
type instance XPChar MplPatternCompiledCase = Void
type instance XPList MplPatternCompiledCase = Void
type instance XPListCons MplPatternCompiledCase = Void


type instance XPSimpleListCons MplPatternCompiledCase = XMplType MplTypeChecked
type instance XPSimpleListEmpty MplPatternCompiledCase = XMplType MplTypeChecked
type instance XPSimpleUnit MplPatternCompiledCase = XMplType MplTypeChecked
---------------------------

type instance XECall MplPatternCompiled = XECall MplTypeChecked

type instance XEObjCall MplPatternCompiled = XEObjCall MplTypeChecked
type instance XERecord MplPatternCompiled = XMplType MplPatternCompiled
type instance XERecordPhrase MplPatternCompiled = MplTypePhrase MplTypeChecked (SeqObjTag CodataDefnTag)
type instance XXExpr MplPatternCompiled = Void

-- built in expression types
type instance XEList MplPatternCompiled = (XMplType MplTypeChecked)
type instance XEString MplPatternCompiled = (XMplType MplTypeChecked)
type instance XEStore MplPatternCompiled = (IdentT, XMplType MplTypeChecked)
type instance XEUnit MplPatternCompiled = (XMplType MplTypeChecked)
type instance XETuple MplPatternCompiled = (XMplType MplTypeChecked)
type instance XEProj MplPatternCompiled = (XMplType MplTypeChecked)
type instance XEBuiltInOp MplPatternCompiled = (XMplType MplTypeChecked)

-- built in expression control
type instance XEIf MplPatternCompiled = XMplType MplTypeChecked
type instance XELet MplPatternCompiled = ()
type instance XEFold MplPatternCompiled = XMplType MplTypeChecked
type instance XEFoldPhrase MplPatternCompiled = 
    -- (MplTypePhrase MplPatternCompiled (SeqObjTag DataDefnTag), XMplType MplPatternCompiled)
    MplTypePhrase MplPatternCompiled (SeqObjTag DataDefnTag)
type instance XEUnfold MplPatternCompiled = XMplType MplTypeChecked
type instance XEUnfoldPhrase MplPatternCompiled = 
    ()
type instance XEUnfoldSubPhrase MplPatternCompiled = 
    MplTypePhrase MplPatternCompiled (SeqObjTag CodataDefnTag)
    -- (MplTypePhrase MplPatternCompiled (SeqObjTag CodataDefnTag), XMplType MplPatternCompiled)
-- type instance XESwitch MplPatternCompiled = XMplType MplPatternCompiled
type instance XESwitch MplPatternCompiled = Void
type instance XEIllegalInstr MplPatternCompiled = XMplType MplTypeChecked

-- Pattern instances..
type instance XMplPattern MplPatternCompiled = MplPattern MplPatternCompiled
type instance XPConstructor MplPatternCompiled = Void
type instance XPSimpleConstructor MplPatternCompiled = Void
type instance XPSimpleConstructorArgs MplPatternCompiled = Void

type instance XPRecord MplPatternCompiled = Void
type instance XPRecordPhrase MplPatternCompiled = Void

type instance XPVar MplPatternCompiled = XMplType MplTypeChecked
type instance XPNull MplPatternCompiled = Void
type instance XXPattern MplPatternCompiled = Void

-- built in..
type instance XPUnit MplPatternCompiled = Void
type instance XPTuple MplPatternCompiled = Void
type instance XPString MplPatternCompiled = Void
type instance XPInt MplPatternCompiled = Void
type instance XPBool MplPatternCompiled = Void
type instance XPChar MplPatternCompiled = Void
type instance XPList MplPatternCompiled = Void
type instance XPListCons MplPatternCompiled = Void

type instance XPSimpleListCons MplPatternCompiled = Void
type instance XPSimpleListEmpty MplPatternCompiled = Void
type instance XPSimpleUnit MplPatternCompiled = Void

-- Process Command
type instance XMplCmd MplPatternCompiled = MplCmd MplPatternCompiled
    -- don't really need this annotation information
type instance XCRun MplPatternCompiled = ()
type instance XCClose MplPatternCompiled = KeyWordNameOcc
type instance XCHalt MplPatternCompiled = KeyWordNameOcc
type instance XCGet MplPatternCompiled = KeyWordNameOcc
type instance XCPut MplPatternCompiled = XCPut MplTypeChecked
type instance XCHCase MplPatternCompiled = XCHCase MplTypeChecked

type instance XCHPut MplPatternCompiled = XCHPut MplTypeChecked
type instance XCSplit MplPatternCompiled = KeyWordNameOcc
type instance XCFork MplPatternCompiled = KeyWordNameOcc
type instance XCId MplPatternCompiled = KeyWordNameOcc
type instance XCIdNeg MplPatternCompiled = KeyWordNameOcc
type instance XCRace MplPatternCompiled = KeyWordNameOcc
type instance XCPlugs MplPatternCompiled = 
    ( KeyWordNameOcc
    , [(IdP MplPatternCompiled, XMplType MplPatternCompiled)])
                                                    -- these are the new plugged channels.
                                                    -- Note that these do not have a polarity 
                                                    -- because it changes based on the phrase
type instance XCCase MplPatternCompiled = ()
type instance XCCasePattern MplPatternCompiled = XCCasePattern MplPatternCompiledCase 
type instance XCCasePattern MplPatternCompiledCase = MplPattern MplPatternCompiledCase

type instance XCSwitch MplPatternCompiled = Void
type instance XCIf MplPatternCompiled = ()
type instance XCHCasePhrase MplPatternCompiled  = MplConcObjDefn MplTypeCheckedPhrase
type instance XCForkPhrase MplPatternCompiled  = [ChP MplPatternCompiled] 
type instance XCPlugPhrase MplPatternCompiled  = ()
type instance XCIllegalInstr MplPatternCompiled  = ()
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
type instance XXType MplPatternCompiled = XXType MplTypeChecked
type instance XTypeIntF MplPatternCompiled = XTypeIntF MplTypeChecked
type instance XTypeCharF MplPatternCompiled = XTypeCharF MplTypeChecked
type instance XTypeDoubleF MplPatternCompiled = XTypeDoubleF MplTypeChecked
-- type instance XTypeStringF MplPatternCompiled = XTypeStringF MplTypeChecked
type instance XTypeUnitF MplPatternCompiled = XTypeUnitF MplTypeChecked
type instance XTypeBoolF MplPatternCompiled = XTypeBoolF MplTypeChecked
type instance XTypeListF MplPatternCompiled = XTypeListF MplTypeChecked
type instance XTypeTupleF MplPatternCompiled = XTypeTupleF MplTypeChecked
type instance XTypeStoreF MplPatternCompiled = XTypeStoreF MplTypeChecked

type instance XTypeGet MplPatternCompiled = XTypeGet MplTypeChecked
type instance XTypePut MplPatternCompiled = XTypePut MplTypeChecked
type instance XTypeTensor MplPatternCompiled = XTypeTensor MplTypeChecked
type instance XTypePar MplPatternCompiled = XTypePar MplTypeChecked
type instance XTypeTopBot MplPatternCompiled = XTypeTopBot MplTypeChecked
type instance XTypeNeg MplPatternCompiled = XTypeNeg MplTypeChecked
type instance XTypeSeqArrF MplPatternCompiled = XTypeSeqArrF MplTypeChecked
type instance XTypeConcArrF MplPatternCompiled = XTypeConcArrF MplTypeChecked

type instance XXMplBuiltInTypesF MplPatternCompiled = XXMplBuiltInTypesF MplTypeChecked

-- Kind info..
-------------------------
type instance XMplKind MplPatternCompiled = MplPrimitiveKind MplPatternCompiled
type instance XSeqKind MplPatternCompiled = XSeqKind MplTypeChecked
type instance XConcKind MplPatternCompiled = XConcKind MplTypeChecked
type instance XArrKind MplPatternCompiled = XArrKind MplTypeChecked
type instance XSeqArgKind MplPatternCompiled = XSeqArgKind MplTypeChecked
type instance XConcArgKind MplPatternCompiled = XConcArgKind MplTypeChecked
type instance XKindVar MplPatternCompiled = XKindVar MplTypeChecked

type instance KindP MplPatternCompiled = KindP MplTypeChecked

type instance XXKind MplPatternCompiled = XXKind MplTypeChecked

