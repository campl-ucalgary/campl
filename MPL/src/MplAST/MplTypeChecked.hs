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
import MplUtil.UniqueSupply 


type IdentT = IdentR

{-
data ChIdentR = ChIdentR {
    _chIdentRIdentR :: IdentR
    , _chIdentRPolarity :: Polarity
}
  deriving Show

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
type instance XCPlugs MplRenamed = (KeyWordNameOcc, [IdP MplRenamed])
                                                    -- these are the new plugged channels.
                                                    -- Note that these do not have a polarity 
                                                    -- because it changes based on the phrase
type instance XCCase MplRenamed = KeyWordNameOcc
type instance XCSwitch MplRenamed = KeyWordNameOcc
type instance XCHCasePhrase MplRenamed  = ()
type instance XCForkPhrase MplRenamed  = [ChP MplRenamed] 
type instance XCPlugPhrase MplRenamed  = ()
type instance XXCmd MplRenamed = Void

-- Type clause (note, we tie the knot here so it is easy to compute all 
-- the type phrases and type clauses when doing totality checks of
-- folds and unfolds along with hcase)
type instance XTypeClauseSpineExt MplRenamed t = ()
type instance XTypeClauseExt MplRenamed t = () -- MplTypeClauseSpine MplRenamed t
type instance XTypePhraseExt MplRenamed  t = ()

type instance XTypePhraseTo MplRenamed t = 
    XMplType MplRenamed

type instance XTypePhraseFrom MplRenamed (SeqObjTag DataDefnTag) = 
    [XMplType MplRenamed]
type instance XTypePhraseFrom MplRenamed (SeqObjTag CodataDefnTag) = 
    ([XMplType MplRenamed], XMplType MplRenamed) -- args ++ [statevar] State var must be the last variable
type instance XTypePhraseFrom MplRenamed (ConcObjTag ProtocolDefnTag) = 
    XMplType MplRenamed
type instance XTypePhraseFrom MplRenamed (ConcObjTag CoprotocolDefnTag) = 
    XMplType MplRenamed

-- Function / process type
type instance XFunType MplRenamed  = Maybe ([IdentR], [XMplType MplRenamed], XMplType MplRenamed)
type instance XProcType MplRenamed = 
    Maybe ([IdentR], [XMplType MplRenamed], [XMplType MplRenamed], [XMplType MplRenamed])

type instance XMplType MplRenamed = MplType MplRenamed
type instance XTypeSeqWithArgs MplRenamed = ()
type instance XTypeSeqVarWithArgs MplRenamed = ()
type instance XTypeConcWithArgs MplRenamed = ()
type instance XTypeConcVarWithArgs  MplRenamed = ()

type instance XTypeVar MplRenamed = () 
type instance XXType MplRenamed = Void
type instance XTypeIntF MplRenamed = NameOcc
type instance XTypeCharF MplRenamed = NameOcc
type instance XTypeDoubleF MplRenamed = NameOcc
type instance XTypeStringF MplRenamed = NameOcc
type instance XTypeUnitF MplRenamed = NameOcc
type instance XTypeBoolF MplRenamed = NameOcc
type instance XTypeListF MplRenamed = NameOcc
type instance XTypeTupleF MplRenamed = NameOcc

type instance XTypeGet MplRenamed = NameOcc
type instance XTypePut MplRenamed = NameOcc
type instance XTypeTensor MplRenamed = NameOcc
type instance XTypePar MplRenamed = NameOcc
type instance XTypeTopBot MplRenamed = NameOcc
type instance XTypeNeg MplRenamed = NameOcc
type instance XTypeSeqArrF MplRenamed = Void
type instance XTypeConcArrF MplRenamed = Void

type instance XXMplBuiltInTypesF MplRenamed = Void
-}
