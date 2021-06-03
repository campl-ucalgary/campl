{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module MplAsmAST.MplAsmCommand where

import Optics
import Data.Functor.Foldable.TH

type family IdP x

-- | TheType . TheConstructor 
data TypeAndSpec x = TypeAndSpec 
    { _typeAndSpecTy :: IdP x
    , _typeAndSpecSpec :: IdP x
    }

$(makeLenses ''TypeAndSpec)

data MplAsmCom x
    = CAssign (IdP x) (MplAsmCom x)
    | CLoad (IdP x)
    | CRet
    | CCall (IdP x) [IdP x]
    | CInt Int
    | CChar Char

    -- and, or other bool operators

    | CEqInt
    | CLeqInt
    | CEqChar
    | CLeqChar

    | CAdd
    | CSub
    | CMul
    -- | data type, handle, arguments
    | CConstructor (TypeAndSpec x) [IdP x]
    -- | data type, handle, arguments, expression to destruct
    | CDestructor (TypeAndSpec x) [IdP x] (IdP x)

    | CCase (IdP x) [LabelledMplSeqComs x]
    | CRecord [LabelledMplSeqComs x]
    | CIf (IdP x) (MplAsmComs x) (MplAsmComs x)

    | CTuple [IdP x]
    | CProj Word (IdP x)


    -- | Concurrent command. @get a on channel@
    | CGet (IdP x) (IdP x)
    -- | Concurrent command. @put a on channel@
    | CPut (IdP x) (IdP x)
    | CHPut (TypeAndSpec x) (IdP x)
    | CHCase (IdP x) [LabelledMplConcComs x]
    | CSplit (IdP x) (IdP x, IdP x)
    | CFork (IdP x) (ForkPhrase x, ForkPhrase x)
    | CPlug [IdP x] (PlugPhrase x, PlugPhrase x)
    | CRun (IdP x) ([IdP x], [IdP x], [IdP x])
    | CId (IdP x, IdP x)
    | CRace [RacePhrase x]

type MplAsmComs x = [MplAsmCom x]
type LabelledMplSeqComs x = (TypeAndSpec x, [IdP x], MplAsmComs x) 
type LabelledMplConcComs x = (TypeAndSpec x, MplAsmComs x) 
-- | @a with a0,a1 : coms @
type ForkPhrase x = (IdP x, [IdP x], MplAsmComs x)
type PlugPhrase x = ([IdP x], MplAsmComs x)
type RacePhrase x = (IdP x, MplAsmComs x)

$(makePrisms ''MplAsmCom)
$(makeBaseFunctor ''MplAsmCom)
