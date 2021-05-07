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
module MplAST.MplNewParsed where

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

import MplParse.Util

data MplNewParsed


type instance IdP MplNewParsed = Spanned String
type instance ChP MplNewParsed = Spanned String
type instance TypeP MplNewParsed = Spanned String

-- definitions.
type instance XDataDefn MplNewParsed  = 
    MplTypeClauseSpine MplNewParsed (SeqObjTag DataDefnTag)
type instance XCodataDefn MplNewParsed  = MplTypeClauseSpine MplNewParsed (SeqObjTag CodataDefnTag)
type instance XProtocolDefn MplNewParsed  = 
    MplTypeClauseSpine MplNewParsed (ConcObjTag ProtocolDefnTag)
type instance XCoprotocolDefn MplNewParsed  = 
    MplTypeClauseSpine MplNewParsed (ConcObjTag CoprotocolDefnTag)

type instance XFunctionDefn MplNewParsed = MplFunction MplNewParsed
type instance XProcessDefn MplNewParsed  = MplProcess MplNewParsed


-- Expression instances
type instance XMplExpr MplNewParsed = MplExpr MplNewParsed
type instance XEPOps MplNewParsed = ()
type instance XEVar MplNewParsed = ()
type instance XEInt MplNewParsed = Span
type instance XEChar MplNewParsed = Span
type instance XEDouble MplNewParsed = Span
type instance XECase MplNewParsed = ()
type instance XEObjCall MplNewParsed = ()
type instance XECall MplNewParsed = ()
type instance XERecord MplNewParsed = Span
type instance XERecordPhrase MplNewParsed = ()
type instance XXExpr MplNewParsed = Void
-- built in expression types
type instance XEList MplNewParsed = Span
type instance XEString MplNewParsed = Span
type instance XEUnit MplNewParsed = Span
type instance XETuple MplNewParsed = Span
type instance XEBuiltInOp MplNewParsed = Span
-- built in expression control
type instance XEIf MplNewParsed = ()
type instance XELet MplNewParsed = ()
type instance XEFold MplNewParsed = ()
type instance XEFoldPhrase MplNewParsed = ()
type instance XEUnfold MplNewParsed = ()
type instance XEUnfoldPhrase MplNewParsed = ()
type instance XEUnfoldSubPhrase MplNewParsed = ()
type instance XESwitch MplNewParsed = ()

-- Pattern instances..
type instance XMplPattern MplNewParsed = MplPattern MplNewParsed
type instance XPConstructor MplNewParsed = ()
type instance XPRecord MplNewParsed = Span
type instance XPRecordPhrase MplNewParsed = ()
type instance XPVar MplNewParsed = ()
type instance XPNull MplNewParsed = Span
type instance XXPattern MplNewParsed = Void
-- built in..
type instance XPUnit MplNewParsed = Span
type instance XPTuple MplNewParsed = Span
type instance XPString MplNewParsed = Span
type instance XPInt MplNewParsed = Span
type instance XPChar MplNewParsed = Span
type instance XPList MplNewParsed = Span
type instance XPListCons MplNewParsed = Span

-- Process Command
type instance XMplCmd MplNewParsed = MplCmd MplNewParsed
type instance XCRun MplNewParsed = ()
type instance XCClose MplNewParsed = Span
type instance XCHalt MplNewParsed = Span
type instance XCGet MplNewParsed = Span
type instance XCPut MplNewParsed = Span
type instance XCHCase MplNewParsed = Span
type instance XCHPut MplNewParsed = Span
type instance XCSplit MplNewParsed = Span
type instance XCFork MplNewParsed = Span
type instance XCId MplNewParsed = Span
type instance XCIdNeg MplNewParsed = Span
type instance XCRace MplNewParsed = Span
type instance XCPlug MplNewParsed = Void
type instance XCPlugs MplNewParsed = 
    (Span, Maybe [(ChP MplNewParsed)])
        -- user can explictly write the channels 
        -- being bound. Note: due to a limitation of bnfc
        -- we can never actually write this
type instance XCCase MplNewParsed = Span
type instance XCSwitch MplNewParsed = Span
type instance XCHCasePhrase MplNewParsed  = ()
type instance XCForkPhrase MplNewParsed  = Maybe [(ChP MplNewParsed)] -- user can supply their own context
type instance XCPlugPhrase MplNewParsed  = () -- user can supply their own context
type instance XXCmd MplNewParsed = Void

-- Type clause
type instance XTypeClauseSpineExt MplNewParsed t = ()
type instance XTypeClauseExt MplNewParsed t = ()
type instance XTypePhraseExt MplNewParsed  t = ()

-- type phrases
type instance XTypePhraseFrom MplNewParsed (SeqObjTag DataDefnTag) = [XMplType MplNewParsed]
type instance XTypePhraseTo MplNewParsed (SeqObjTag DataDefnTag) = IdP MplNewParsed

type instance XTypePhraseFrom MplNewParsed (SeqObjTag CodataDefnTag) = 
    ([XMplType MplNewParsed], IdP MplNewParsed)
        -- args ++ [statevar]
        -- State var must be the last variable
type instance XTypePhraseTo MplNewParsed (SeqObjTag CodataDefnTag) = XMplType MplNewParsed

type instance XTypePhraseFrom MplNewParsed (ConcObjTag ProtocolDefnTag) = XMplType MplNewParsed
type instance XTypePhraseTo MplNewParsed (ConcObjTag ProtocolDefnTag) = IdP MplNewParsed

type instance XTypePhraseFrom MplNewParsed (ConcObjTag CoprotocolDefnTag) = IdP MplNewParsed
type instance XTypePhraseTo MplNewParsed (ConcObjTag CoprotocolDefnTag) = XMplType MplNewParsed

-- Function / process type
type instance XFunType MplNewParsed  = Maybe ([XMplType MplNewParsed], XMplType MplNewParsed)
type instance XProcType MplNewParsed = 
    Maybe ([XMplType MplNewParsed], [XMplType MplNewParsed], [XMplType MplNewParsed])

type instance XMplType MplNewParsed = MplType MplNewParsed
type instance XTypeSeqWithArgs MplNewParsed = Void
type instance XTypeSeqVarWithArgs MplNewParsed = ()

type instance XTypeConcWithArgs MplNewParsed = Void
type instance XTypeConcVarWithArgs  MplNewParsed = ()

type instance XTypeVar MplNewParsed = ()
type instance XTypeWithNoArgs MplNewParsed = Void
type instance XXType MplNewParsed = Void
type instance XTypeIntF MplNewParsed = NameOcc
type instance XTypeCharF MplNewParsed = NameOcc
type instance XTypeDoubleF MplNewParsed = NameOcc
type instance XTypeStringF MplNewParsed = NameOcc
type instance XTypeUnitF MplNewParsed = NameOcc
type instance XTypeBoolF MplNewParsed = NameOcc
type instance XTypeListF MplNewParsed = NameOcc
type instance XTypeTupleF MplNewParsed = NameOcc

type instance XTypeGet MplNewParsed = Void
type instance XTypePut MplNewParsed = Void
type instance XTypeTensor MplNewParsed = Span
type instance XTypePar MplNewParsed = Span
type instance XTypeTopBot MplNewParsed = Void
type instance XTypeNeg MplNewParsed = Void
type instance XTypeSeqArrF MplNewParsed = Void
type instance XTypeConcArrF MplNewParsed = Void

type instance XXMplBuiltInTypesF MplNewParsed = Void
