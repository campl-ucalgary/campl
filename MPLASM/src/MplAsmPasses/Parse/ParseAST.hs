{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module MplAsmPasses.Parse.ParseAST where

import MplAsmAST.MplAsmCore
import qualified MplAsmLanguage.AbsMPLASM as B
import Data.Coerce
import Optics
import Data.Function

{- | declarations of data to configure the asm syntax tree. -}
data MplAsmParsed 
type instance IdP MplAsmParsed  = Ident

newtype Ident = Ident { _identStr :: String }
  deriving (Show, Eq, Ord)

data LocatedIdent = LocatedIdent 
    { locatedIdentIdent :: Ident 
    , locatedIdentLoc :: RowCol
    }

type RowCol = (Int, Int)

toIdent :: forall t. Coercible t (RowCol, String) => t -> Ident
toIdent t = case (coerce :: t -> (RowCol, String))  t of
    (_rowcol, str) -> Ident str 

toRowCol :: forall t. Coercible t (RowCol, String) => t -> RowCol
toRowCol t = case (coerce :: t -> (RowCol, String))  t of
    (rowcol, _str) -> rowcol

pIntToInt :: B.PInteger -> Int
pIntToInt = read . snd . (coerce :: B.PInteger -> ((Int, Int), String))

pCharToChar :: B.Character -> Char
pCharToChar = read . snd . (coerce :: B.Character -> ((Int, Int), String))

pBoolToBool :: B.BBool -> Bool
pBoolToBool = read . snd . (coerce :: B.BBool -> ((Int, Int), String))

$(makeClassy ''Ident)

type instance XCAssign MplAsmParsed = RowCol
type instance XCLoad MplAsmParsed = RowCol
type instance XCRet MplAsmParsed = RowCol
type instance XCCall MplAsmParsed = RowCol
type instance XCInt MplAsmParsed = RowCol
type instance XCChar MplAsmParsed = RowCol
type instance XCBool MplAsmParsed = RowCol
type instance XCEqInt MplAsmParsed = RowCol

type instance XCLeqInt MplAsmParsed = RowCol
type instance XCEqChar MplAsmParsed = RowCol
type instance XCLeqChar MplAsmParsed = RowCol

type instance XCAdd MplAsmParsed = RowCol
type instance XCSub MplAsmParsed = RowCol
type instance XCMul MplAsmParsed = RowCol
type instance XCConstructor MplAsmParsed = RowCol
type instance XCDestructor MplAsmParsed = RowCol

type instance XCCase MplAsmParsed = RowCol
type instance XCRecord MplAsmParsed = RowCol
type instance XCIf MplAsmParsed = RowCol

type instance XCTuple MplAsmParsed = RowCol
type instance XCProj MplAsmParsed = RowCol

type instance XCGet MplAsmParsed = RowCol
type instance XCPut MplAsmParsed = RowCol
type instance XCHPut MplAsmParsed = RowCol
type instance XCHCase MplAsmParsed = RowCol
type instance XCSplit MplAsmParsed = RowCol
type instance XCFork MplAsmParsed = RowCol
type instance XCPlug MplAsmParsed = RowCol
type instance XCRun MplAsmParsed = RowCol
type instance XCId MplAsmParsed = RowCol
type instance XCRace MplAsmParsed = RowCol
type instance XCClose MplAsmParsed = RowCol
type instance XCHalt MplAsmParsed = RowCol
