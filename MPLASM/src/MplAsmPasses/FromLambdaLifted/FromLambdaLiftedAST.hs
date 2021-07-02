{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module MplAsmPasses.FromLambdaLifted.FromLambdaLiftedAST where

-- front end
import MplAST.MplCore 
import MplAST.MplTypeChecked 
import MplUtil.UniqueSupply

-- Assembler
import qualified MplAsmAST.MplAsmProg as Asm
import qualified MplAsmAST.MplAsmCore as Asm
import qualified MplAsmPasses.PassesErrorsPprint as Asm

import Data.Text.Prettyprint.Doc

data MplAsmFromLambdaLifted 

{- | the usual configuring the AST -}
type instance Asm.IdP MplAsmFromLambdaLifted = Asm.Name

type instance Asm.XCAssign MplAsmFromLambdaLifted = ()
type instance Asm.XCLoad MplAsmFromLambdaLifted = ()
type instance Asm.XCStore MplAsmFromLambdaLifted = ()
type instance Asm.XCRet MplAsmFromLambdaLifted = ()
type instance Asm.XCCall MplAsmFromLambdaLifted = ()
type instance Asm.XCInt MplAsmFromLambdaLifted = ()
type instance Asm.XCChar MplAsmFromLambdaLifted = ()
type instance Asm.XCBool MplAsmFromLambdaLifted = ()
type instance Asm.XCEqInt MplAsmFromLambdaLifted = ()

type instance Asm.XCLeqInt MplAsmFromLambdaLifted = ()
type instance Asm.XCEqChar MplAsmFromLambdaLifted = ()
type instance Asm.XCLeqChar MplAsmFromLambdaLifted = ()

type instance Asm.XCAdd MplAsmFromLambdaLifted = ()
type instance Asm.XCSub MplAsmFromLambdaLifted = ()
type instance Asm.XCMul MplAsmFromLambdaLifted = ()
type instance Asm.XCConstructor MplAsmFromLambdaLifted = ()
type instance Asm.XCDestructor MplAsmFromLambdaLifted = ()

type instance Asm.XCCase MplAsmFromLambdaLifted = ()
type instance Asm.XCRecord MplAsmFromLambdaLifted = ()
type instance Asm.XCIf MplAsmFromLambdaLifted = ()

type instance Asm.XCTuple MplAsmFromLambdaLifted = ()
type instance Asm.XCProj MplAsmFromLambdaLifted = ()

type instance Asm.XCGet MplAsmFromLambdaLifted = ()
type instance Asm.XCPut MplAsmFromLambdaLifted = ()
type instance Asm.XCHPut MplAsmFromLambdaLifted = ()
type instance Asm.XCHCase MplAsmFromLambdaLifted = ()
type instance Asm.XCSplit MplAsmFromLambdaLifted = ()
type instance Asm.XCFork MplAsmFromLambdaLifted = ()
type instance Asm.XCPlug MplAsmFromLambdaLifted = ()
type instance Asm.XCRun MplAsmFromLambdaLifted = ()
type instance Asm.XCId MplAsmFromLambdaLifted = ()
type instance Asm.XCRace MplAsmFromLambdaLifted = ()
type instance Asm.XCClose MplAsmFromLambdaLifted = ()
type instance Asm.XCHalt MplAsmFromLambdaLifted = ()

instance Pretty (Asm.TypeAndSpec MplAsmFromLambdaLifted) where
    pretty (Asm.TypeAndSpec tp spec) = pretty tp <> dot <> pretty spec
