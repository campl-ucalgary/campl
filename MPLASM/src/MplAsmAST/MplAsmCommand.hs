{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module MplAsmAST.MplAsmCommand where

import Data.Functor.Foldable.TH
import Data.Kind
import qualified MplMach.MplMachTypes as MplMach
import Optics

type family IdP x

type family XCAssign x

type family XCLoad x

type family XCStore x

type family XCRet x

type family XCCall x

type family XCInt x

type family XCChar x

type family XCBool x

type family XCIntCmp x

type family XCEqChar x

type family XCLeqChar x

type family XCIntArith x

type family XCConstructor x

type family XCDestructor x

type family XCCase x

type family XCRecord x

type family XCIf x

type family XCTuple x

type family XCProj x

type family XCGet x

type family XCPut x

type family XCHPut x

type family XCHCase x

type family XCSplit x

type family XCFork x

type family XCPlug x

type family XCRun x

type family XCId x

type family XCRace x

type family XCClose x

type family XCHalt x

type family XCStoreProc x

type ForallMplAsmCom :: (Type -> Constraint) -> Type -> Constraint
type ForallMplAsmCom c x =
  ( c (IdP x),
    c (XCAssign x),
    c (XCLoad x),
    c (XCStore x),
    c (XCRet x),
    c (XCCall x),
    c (XCInt x),
    c (XCChar x),
    c (XCBool x),
    c (XCIntCmp x),
    c (XCEqChar x),
    c (XCLeqChar x),
    c (XCConstructor x),
    c (XCDestructor x),
    c (XCCase x),
    c (XCRecord x),
    c (XCIf x),
    c (XCTuple x),
    c (XCProj x),
    c (XCGet x),
    c (XCPut x),
    c (XCHPut x),
    c (XCHCase x),
    c (XCSplit x),
    c (XCFork x),
    c (XCPlug x),
    c (XCRun x),
    c (XCId x),
    c (XCRace x),
    c (XCClose x),
    c (XCHalt x)
  )

-- | TheType . TheConstructor
data TypeAndSpec x = TypeAndSpec
  { _typeAndSpecTy :: IdP x,
    _typeAndSpecSpec :: IdP x
  }

deriving instance (Show (IdP x)) => Show (TypeAndSpec x)

$(makeLenses ''TypeAndSpec)

data MplAsmCom x
  = CAssign (XCAssign x) (IdP x) (MplAsmCom x)
  | CLoad (XCLoad x) (IdP x)
  | -- | stores the top element of the stack to the identifier
    CStore (XCStore x) (IdP x)
  | CRet (XCRet x)
  | CCall (XCCall x) (IdP x) [IdP x]
  | CInt (XCInt x) Int
  | CChar (XCChar x) Char
  | CBool (XCBool x) Bool
  | -- TODO: These SHOULD NOT all use the Eq int type family
    -- and, or other bool operators
    CEqBool (XCIntCmp x)
  | CEqInt (XCIntCmp x)
  | CLtInt (XCIntCmp x)
  | CLeqInt (XCIntCmp x)
  | CGtInt (XCIntCmp x)
  | CGeqInt (XCIntCmp x)
  | CEqChar (XCEqChar x)
  | CLeqChar (XCLeqChar x)
  | CAddInt (XCIntArith x)
  | CSubInt (XCIntArith x)
  | CMulInt (XCIntArith x)
  | CDivInt (XCIntArith x)
  | -- | data type, handle, arguments
    CConstructor (XCConstructor x) (TypeAndSpec x) [IdP x]
  | -- | data type, handle, arguments, expression to destruct
    CDestructor (XCDestructor x) (TypeAndSpec x) [IdP x] (IdP x)
  | CCase (XCCase x) (IdP x) [LabelledMplSeqComs x]
  | CRecord (XCRecord x) [LabelledMplSeqComs x]
  | CIf (XCIf x) (IdP x) (MplAsmComs x) (MplAsmComs x)
  | CTuple (XCTuple x) [IdP x]
  | CProj (XCProj x) Int (IdP x)
  | CStoreProc (XCStoreProc x) (Either (IdP x) (StorePhrase x))
  | -- | Concurrent command. @get a on channel@
    CGet (XCGet x) (IdP x) (IdP x)
  | -- | Concurrent command. @put a on channel@
    CPut (XCPut x) (IdP x) (IdP x)
  | CHPut (XCHPut x) (TypeAndSpec x) (IdP x)
  | CSHPut (XCHPut x) MplAsmServices (IdP x)
  | CHCase (XCHCase x) (IdP x) [LabelledMplConcComs x]
  | CSplit (XCSplit x) (IdP x) (IdP x, IdP x)
  | CFork (XCFork x) (IdP x) (ForkPhrase x, ForkPhrase x)
  | CPlug (XCPlug x) [IdP x] (PlugPhrase x, PlugPhrase x)
  | CRun (XCRun x) Bool (IdP x) ([IdP x], [IdP x], [IdP x])
  | CId (XCId x) (IdP x, IdP x)
  | CRace (XCRace x) [RacePhrase x]
  | CClose (XCRace x) (IdP x)
  | CHalt (XCRace x) (IdP x)

-- | These are the possible external services.
data MplAsmServices
  = SHGetInt
  | SHPutInt
  | SHGetString
  | SHPutString
  | SHGetChar
  | SHPutChar
  | SHTimeOut
  | SHSplitNegStringTerm
  | SHClose
  deriving (Show)

asmServiceToMachService ::
  MplAsmServices ->
  MplMach.SInstr
asmServiceToMachService = \case
  SHGetInt -> MplMach.SHGetInt
  SHPutInt -> MplMach.SHPutInt
  SHGetChar -> MplMach.SHGetChar
  SHPutChar -> MplMach.SHPutChar
  SHGetString -> MplMach.SHGetString
  SHPutString -> MplMach.SHPutString
  SHTimeOut -> MplMach.SHTimeOut
  SHSplitNegStringTerm -> MplMach.SHSplitNegStringTerm
  SHClose -> MplMach.SHClose

type MplAsmComs x = [MplAsmCom x]

type LabelledMplSeqComs x = (TypeAndSpec x, [IdP x], MplAsmComs x)

type LabelledMplConcComs x = (TypeAndSpec x, MplAsmComs x)

-- | @a with a0,a1 : coms @
type ForkPhrase x = (IdP x, [IdP x], MplAsmComs x)

type PlugPhrase x = (([IdP x], [IdP x]), MplAsmComs x)

type RacePhrase x = (IdP x, MplAsmComs x)

type StorePhrase x = (([IdP x], [IdP x], [IdP x]), MplAsmComs x)

$(makePrisms ''MplAsmCom)
$(makeBaseFunctor ''MplAsmCom)
