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
module MplAST.MplLambdaLifted where

import Optics
import Optics.State.Operators
import Data.Void

import Data.Function

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE 

import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import MplAST.MplPatternCompiled
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


type instance IdP MplLambdaLifted = IdP MplTypeChecked 
type instance ChP MplLambdaLifted = ChP MplTypeChecked 
type instance TypeP MplLambdaLifted = TypeP MplTypeChecked 

type instance XMplType MplLambdaLifted = MplType MplTypeChecked 

-- definitions.
type instance XDataDefn MplLambdaLifted  = 
    MplTypeClauseSpine MplLambdaLifted (SeqObjTag DataDefnTag)
type instance XCodataDefn MplLambdaLifted  = 
    MplTypeClauseSpine MplLambdaLifted (SeqObjTag CodataDefnTag)
type instance XProtocolDefn MplLambdaLifted  = 
    MplTypeClauseSpine MplLambdaLifted (ConcObjTag ProtocolDefnTag)
type instance XCoprotocolDefn MplLambdaLifted  = 
    MplTypeClauseSpine MplLambdaLifted (ConcObjTag CoprotocolDefnTag)

type instance XFunctionDefn MplLambdaLifted = MplFunction MplLambdaLifted
type instance XProcessDefn MplLambdaLifted  = MplProcess MplLambdaLifted 

-- Expression instances
type instance XMplExpr MplLambdaLifted = MplExpr MplLambdaLifted
type instance XEPOps MplLambdaLifted = XEPOps MplTypeChecked
type instance XEVar MplLambdaLifted = XEVar MplTypeChecked

type instance XEInt MplLambdaLifted = XMplType MplTypeChecked
type instance XEChar MplLambdaLifted = XMplType MplTypeChecked
type instance XEBool MplLambdaLifted = XMplType MplTypeChecked
type instance XEDouble MplLambdaLifted = XMplType MplTypeChecked
type instance XECase MplLambdaLifted = XMplType MplTypeChecked
type instance XECasePattern MplLambdaLifted =  MplPattern MplPatternCompiledCase

type instance XECall MplLambdaLifted = XECall MplTypeChecked

type instance XEObjCall MplLambdaLifted = XEObjCall MplTypeChecked
type instance XERecord MplLambdaLifted = XMplType MplLambdaLifted
type instance XERecordPhrase MplLambdaLifted = MplTypePhrase MplTypeChecked (SeqObjTag CodataDefnTag)
type instance XXExpr MplLambdaLifted = Void

-- built in expression types
type instance XEList MplLambdaLifted = (XMplType MplTypeChecked)
type instance XEStore MplLambdaLifted = (XMplType MplTypeChecked)
type instance XEString MplLambdaLifted = (XMplType MplTypeChecked)
type instance XEUnit MplLambdaLifted = (XMplType MplTypeChecked)
type instance XETuple MplLambdaLifted = (XMplType MplTypeChecked)
type instance XEProj MplLambdaLifted = (XMplType MplTypeChecked)
type instance XEBuiltInOp MplLambdaLifted = (XMplType MplTypeChecked)

-- built in expression control
type instance XEIf MplLambdaLifted = XMplType MplTypeChecked
type instance XELet MplLambdaLifted = Void
type instance XEFold MplLambdaLifted = XMplType MplTypeChecked
type instance XEFoldPhrase MplLambdaLifted = 
    -- (MplTypePhrase MplLambdaLifted (SeqObjTag DataDefnTag), XMplType MplLambdaLifted)
    MplTypePhrase MplLambdaLifted (SeqObjTag DataDefnTag)
type instance XEUnfold MplLambdaLifted = XMplType MplTypeChecked
type instance XEUnfoldPhrase MplLambdaLifted = 
    ()
type instance XEUnfoldSubPhrase MplLambdaLifted = 
    MplTypePhrase MplLambdaLifted (SeqObjTag CodataDefnTag)
    -- (MplTypePhrase MplLambdaLifted (SeqObjTag CodataDefnTag), XMplType MplLambdaLifted)
-- type instance XESwitch MplLambdaLifted = XMplType MplLambdaLifted
type instance XESwitch MplLambdaLifted = Void
type instance XEIllegalInstr MplLambdaLifted = XEIllegalInstr MplPatternCompiled

-- Pattern instances..
type instance XMplPattern MplLambdaLifted = MplPattern MplPatternCompiled
type instance XPConstructor MplLambdaLifted = Void
type instance XPSimpleConstructor MplLambdaLifted = Void
type instance XPSimpleConstructorArgs MplLambdaLifted = Void

type instance XPRecord MplLambdaLifted = Void
type instance XPRecordPhrase MplLambdaLifted = Void

type instance XPVar MplLambdaLifted = XMplType MplTypeChecked
type instance XPNull MplLambdaLifted = Void
type instance XXPattern MplLambdaLifted = Void

-- built in..
type instance XPUnit MplLambdaLifted = Void
type instance XPTuple MplLambdaLifted = Void
type instance XPString MplLambdaLifted = Void
type instance XPInt MplLambdaLifted = Void
type instance XPBool MplLambdaLifted = Void
type instance XPChar MplLambdaLifted = Void
type instance XPList MplLambdaLifted = Void
type instance XPListCons MplLambdaLifted =Void

type instance XPSimpleListCons MplLambdaLifted = Void
type instance XPSimpleListEmpty MplLambdaLifted = Void
type instance XPSimpleUnit MplLambdaLifted = Void

-- Process Command
type instance XMplCmd MplLambdaLifted = MplCmd MplLambdaLifted
type instance XCRun MplLambdaLifted = XCRun MplPatternCompiled
type instance XCClose MplLambdaLifted = KeyWordNameOcc
type instance XCHalt MplLambdaLifted = KeyWordNameOcc
type instance XCGet MplLambdaLifted = KeyWordNameOcc
type instance XCPut MplLambdaLifted = XCPut MplTypeChecked
type instance XCHCase MplLambdaLifted = XCHCase MplTypeChecked

type instance XCHPut MplLambdaLifted = XCHPut MplTypeChecked
type instance XCSplit MplLambdaLifted = KeyWordNameOcc
type instance XCFork MplLambdaLifted = KeyWordNameOcc
type instance XCId MplLambdaLifted = KeyWordNameOcc
type instance XCIdNeg MplLambdaLifted = KeyWordNameOcc
type instance XCRace MplLambdaLifted = KeyWordNameOcc
type instance XCPlugs MplLambdaLifted = 
    ( KeyWordNameOcc
    , [(IdP MplLambdaLifted, (XMplType MplLambdaLifted))])
                                                    -- these are the new plugged channels.
                                                    -- Note that these do not have a polarity 
                                                    -- because it changes based on the phrase
type instance XCCase MplLambdaLifted = ()
type instance XCCasePattern MplLambdaLifted = XCCasePattern MplPatternCompiledCase 
type instance XCCasePattern MplPatternCompiledCase = MplPattern MplPatternCompiledCase

type instance XCSwitch MplLambdaLifted = Void
type instance XCIf MplLambdaLifted = ()
type instance XCHCasePhrase MplLambdaLifted  = MplConcObjDefn MplTypeCheckedPhrase
type instance XCForkPhrase MplLambdaLifted  = [ChP MplLambdaLifted] 
type instance XCPlugPhrase MplLambdaLifted  = ()
type instance XCIllegalInstr MplLambdaLifted  = ()
type instance XXCmd MplLambdaLifted = Void

type instance XTypeClauseSpineExt MplLambdaLifted t = ()
type instance XTypeClauseExt MplLambdaLifted t = MplTypeClauseSpine MplTypeChecked t
-- type instance XTypeClauseExt MplLambdaLifted t = ()
type instance XTypePhraseExt MplLambdaLifted t = MplTypeClause MplTypeChecked t
-- type instance XTypePhraseExt MplTypeChecked t = ()

type instance XTypePhraseTo MplLambdaLifted t = 
    XMplType MplTypeChecked
type instance XTypePhraseFrom MplLambdaLifted (SeqObjTag DataDefnTag) = 
    [XMplType MplTypeChecked]
type instance XTypePhraseFrom MplLambdaLifted (SeqObjTag CodataDefnTag) = 
    ([XMplType MplTypeChecked], XMplType MplTypeChecked) -- args ++ [statevar] State var must be the last variable
type instance XTypePhraseFrom MplLambdaLifted (ConcObjTag ProtocolDefnTag) = 
    XMplType MplTypeChecked
type instance XTypePhraseFrom MplLambdaLifted (ConcObjTag CoprotocolDefnTag) = 
    XMplType MplTypeChecked

-- Function / process type
type instance XFunType MplLambdaLifted  = 
    ([TypeP MplTypeChecked], [XMplType MplTypeChecked], XMplType MplTypeChecked)
type instance XProcType MplLambdaLifted = 
    ([TypeP MplLambdaLifted], [XMplType MplLambdaLifted], [XMplType MplLambdaLifted], [XMplType MplLambdaLifted])

type instance XTypeSeqWithArgs MplLambdaLifted = MplSeqObjDefn MplLambdaLifted
type instance XTypeSeqVarWithArgs MplLambdaLifted = Void -- higher kinded types are not allowed (for now)
type instance XTypeConcWithArgs MplLambdaLifted = 
    MplConcObjDefn MplLambdaLifted
type instance XTypeConcVarWithArgs  MplLambdaLifted = Void -- higher kinded types are not allowed (for now)

type instance XTypeVar MplLambdaLifted = 
    Maybe (XMplKind MplLambdaLifted)
type instance XTypeWithNoArgs MplLambdaLifted = 
    MplObjectDefn MplLambdaLifted
type instance XXType MplLambdaLifted = XXType MplTypeChecked
type instance XTypeIntF MplLambdaLifted = XTypeIntF MplTypeChecked
type instance XTypeCharF MplLambdaLifted = XTypeCharF MplTypeChecked
type instance XTypeDoubleF MplLambdaLifted = XTypeDoubleF MplTypeChecked
-- type instance XTypeStringF MplLambdaLifted = XTypeStringF MplTypeChecked
type instance XTypeUnitF MplLambdaLifted = XTypeUnitF MplTypeChecked
type instance XTypeBoolF MplLambdaLifted = XTypeBoolF MplTypeChecked
type instance XTypeListF MplLambdaLifted = XTypeListF MplTypeChecked
type instance XTypeTupleF MplLambdaLifted = XTypeTupleF MplTypeChecked
type instance XTypeStoreF MplLambdaLifted = XTypeStoreF MplTypeChecked

type instance XTypeGet MplLambdaLifted = XTypeGet MplTypeChecked
type instance XTypePut MplLambdaLifted = XTypePut MplTypeChecked
type instance XTypeTensor MplLambdaLifted = XTypeTensor MplTypeChecked
type instance XTypePar MplLambdaLifted = XTypePar MplTypeChecked
type instance XTypeTopBot MplLambdaLifted = XTypeTopBot MplTypeChecked
type instance XTypeNeg MplLambdaLifted = XTypeNeg MplTypeChecked
type instance XTypeSeqArrF MplLambdaLifted = XTypeSeqArrF MplTypeChecked
type instance XTypeConcArrF MplLambdaLifted = XTypeConcArrF MplTypeChecked

type instance XXMplBuiltInTypesF MplLambdaLifted = XXMplBuiltInTypesF MplTypeChecked

-- Kind info..
-------------------------
type instance XMplKind MplLambdaLifted = MplPrimitiveKind MplLambdaLifted
type instance XSeqKind MplLambdaLifted = XSeqKind MplTypeChecked
type instance XConcKind MplLambdaLifted = XConcKind MplTypeChecked
type instance XArrKind MplLambdaLifted = XArrKind MplTypeChecked
type instance XSeqArgKind MplLambdaLifted = XSeqArgKind MplTypeChecked
type instance XConcArgKind MplLambdaLifted = XConcArgKind MplTypeChecked
type instance XKindVar MplLambdaLifted = XKindVar MplTypeChecked

type instance KindP MplLambdaLifted = KindP MplTypeChecked

type instance XXKind MplLambdaLifted = XXKind MplTypeChecked

