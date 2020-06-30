{-# LANGUAGE ViewPatterns #-}
module AMPLAST where

import Language.AbsAMPL

import Data.Maybe
import Data.Tuple
import Data.Coerce
import Text.Read ( readMaybe )
import Control.Arrow

pIdentToIdent :: PIdent -> Ident
pIdentToIdent = 
    swap . (coerce :: PIdent -> ((Int, Int), String))

uIdentToIdent :: UIdent -> Ident
uIdentToIdent = 
    swap . (coerce :: UIdent -> ((Int, Int), String))

pIntegerToWord :: PInteger -> Word 
pIntegerToWord (PInteger (pos, str)) = 
    fromMaybe
        (error ("Internal error when trying to read PInteger \"" ++ str ++ "\" at " ++ show pos))
        (readMaybe str)


type RowColPos = (Int, Int)
type Ident = (String, RowColPos)
type InstrIdent = Ident

-- By convention, we have Constructor (for the 
-- insturction) and the Ident for that constructor
-- immediately follows
type ACom = AComIdents InstrIdent Ident

data AComIdents instrident ident = 
    AAssign ident (AComIdents instrident ident)
    | AStore instrident ident
    | ALoad instrident ident
    | ARet instrident
    | ACall instrident ident [ident]
    | AInt instrident Int
    | AChar instrident Char

    | AString instrident String
    | AToStr instrident 
    | AToInt instrident 

    | AAnd instrident
    | AOr instrident

    | AAppend instrident

    | ATrue instrident 
    | AFalse instrident 

    | ALeqInt instrident
    | AEqInt instrident

    | AEqChar instrident
    | ALeqChar instrident

    | AEqString instrident
    | ALeqString instrident

    | AConcat instrident Int

    | AAddInt instrident
    | ASubInt instrident
    | AMulInt instrident
    | ADivInt instrident
    | AModInt instrident

    | AConstructor (ident, ident)
    | AConstructorArgs ((ident, ident), [ident])

    | ACase instrident (ident, [ALabelComsIdents instrident ident])

    | AIf instrident (ident ,([AComIdents instrident ident], [AComIdents instrident ident]))

    | ARecord instrident [ALabelComsIdents instrident ident]
    | ADest (ident, ident) ident
    | ADestArgs ((ident, ident), [ident]) ident

    | AGet instrident ident ident
    | AHPut instrident (ident, ident) ident
    | AHCase instrident (ident, [ALabelComsIdents instrident ident])

    | APut instrident ident ident

    | ASplit instrident ident ident ident
    | AFork instrident ident 
        ((ident, [ident]), [AComIdents instrident ident])
        ((ident, [ident]), [AComIdents instrident ident])
    | APlug instrident [ident]
        ([ident], [AComIdents instrident ident])
        ([ident], [AComIdents instrident ident])
    | ARun instrident ident 
        ([ident], ([ident], [ident]))
    | AId instrident (ident, ident)
    | ARace instrident [(ident, [AComIdents instrident ident])]
    | AClose instrident ident
    | AHalt instrident ident

    | AProd [ident]
    | AProdElem instrident Word

    | AErrMsg String
  deriving (Show, Eq)

type ALabelComs = ALabelComsIdents InstrIdent Ident
data ALabelComsIdents instrident ident =
    ALabelComs (ident, ident) [AComIdents instrident ident]
    | ALabelComsArgs ((ident, ident), [ident]) [AComIdents instrident ident]
  deriving (Show, Eq)

translateCOMToACom :: COM -> ACom
translateCOMToACom n = case n of
    AC_ASSIGN a b -> AAssign (pIdentToIdent a) (translateCOMToACom b)
    AC_STOREf (Store a) b -> AStore (swap a) (pIdentToIdent b)
    AC_LOADf (Load a) b -> ALoad (swap a) (pIdentToIdent b)
    AC_RET (Ret a) -> ARet (swap a)
    AC_CALLf (Call a) b cs -> ACall (swap a) (pIdentToIdent b) (map pIdentToIdent cs)
    AC_INT (ConstInt a) v -> 
            let v' = case v of 
                        Positive k -> fromIntegral (pIntegerToWord k) 
                        Negative k -> negate (fromIntegral (pIntegerToWord k))
            in AInt (swap a) v'
    AC_CHAR (ConstChar a) (Character (second (tail . init) -> (_, v))) -> 
            -- tail . init is used to remove the single quote '
            let v' = case v of
                        "\\\\" -> '\\' 
                        "\'" -> '\'' 
                        "\n" -> '\n' 
                        "\t" -> '\t' 
                        [c] -> c
            in AChar (swap a) v'
    AC_STRING (ConstString a) str ->
        AString (swap a) str
    AC_TOSTR (ToStr a) ->
        AToStr (swap a)
    AC_TOINT (ToInt a) -> 
        AToInt (swap a)
    AC_AND (And a) ->
        AAnd (swap a)
    AC_OR (Or a) ->
        AOr (swap a)
    AC_APPEND (Append a) ->
        AAppend (swap a)
    AC_TRUE (BTrue a) ->
        ATrue (swap a)
    AC_FALSE (BFalse a) ->
        AFalse (swap a)
    AC_UNSTRING (Unstring a) ->
        error "no AC_UNSTRING instance"
    AC_LEQ (LeqI a) ->
        ALeqInt (swap a)
    AC_EQ (EqI a) ->
        AEqInt (swap a)
    AC_LEQC (Leqc a) ->
        ALeqChar (swap a)
    AC_EQC (Eqc a) ->
        AEqChar (swap a)

    AC_LEQS (Leqs a) ->
        error "no AC_LEQS instance"
    AC_EQS (Eqs a) -> 
        error "no AC_EQS instance"
    AC_CONCAT (ConcatS a) b ->
        error "no AC_CONCAT instance"
        
    AC_ADD (Add a) ->
        AAddInt (swap a)
    AC_SUB (Subtract a) ->
        ASubInt (swap a)
    AC_MUL (Mul a) ->
        AMulInt (swap a)
    AC_DIVQ (Quot a) ->
        AModInt (swap a)
    AC_DIVR (Rem a) ->
        ADivInt (swap a)
    AC_CONS (Cons a) b c ->
        error "no AC_CONS instance"

    AC_STRUCT a b ->
        AConstructor (uIdentToIdent a, uIdentToIdent b)
    AC_STRUCTAS a b args ->
        AConstructorArgs ((uIdentToIdent a, uIdentToIdent b), map pIdentToIdent args)

    AC_CASEf (Case a) b cs ->
        ACase (swap a) (pIdentToIdent b, map translateLABELCOMStoALabelComs cs)
    AC_IF (If a) b (Prog cs) (Prog ds) ->
        AIf (swap a) (pIdentToIdent b
            , (map translateCOMToACom cs , map translateCOMToACom ds))
    AC_RECORDf (Rec a) bs ->
        ARecord (swap a) (map translateLABELCOMStoALabelComs bs)

    AC_DEST a b c ->
        ADest (uIdentToIdent a, uIdentToIdent b) (pIdentToIdent c)
    AC_DESTAS a b cs d ->
        ADestArgs ((uIdentToIdent a, uIdentToIdent b), map pIdentToIdent cs) (pIdentToIdent d)
    AC_GETf (Get a) b c ->
        AGet (swap a) (pIdentToIdent b) (pIdentToIdent c)
    AC_HPUTf (Hput a) b c d ->
        AHPut (swap a) (uIdentToIdent b, uIdentToIdent c) (pIdentToIdent d)

    AC_HCASEf (Hcase a) b cs ->
        AHCase (swap a) (pIdentToIdent b, map translateLABELCOMStoALabelComs cs)

    AC_PUTf (Put a) b c ->
        APut (swap a) (pIdentToIdent b) (pIdentToIdent c)

    AC_SPLITf (Split a) b c d ->
        ASplit (swap a) (pIdentToIdent b) (pIdentToIdent c) (pIdentToIdent d) 

    AC_FORKf (Fork a) b a1 a1s (Prog as) b1 b1s (Prog bs)  -> 
        AFork (swap a) (pIdentToIdent b)
            ((pIdentToIdent a1, map pIdentToIdent a1s), map translateCOMToACom as)
            ((pIdentToIdent b1, map pIdentToIdent b1s), map translateCOMToACom bs)

    AC_PLUGf (Plug a) as a1s (Prog a2s) b1s (Prog b2s) ->
        APlug (swap a) (map pIdentToIdent as)
            (map pIdentToIdent a1s, map translateCOMToACom a2s)
            (map pIdentToIdent b1s, map translateCOMToACom b2s)

    AC_RUNf (Run a) b cs ds es ->
        ARun (swap a) (pIdentToIdent b)
            (map pIdentToIdent cs, (map pIdentToIdent ds, map pIdentToIdent es))

    AC_IDF a (Ch_Id b) c ->
        AId (swap b) (pIdentToIdent a, pIdentToIdent c)

    AC_RACE (Race a) bs -> 
        ARace (swap a) (map f bs)
      where
        f (Races a (Prog coms)) = (pIdentToIdent a, map translateCOMToACom coms)

    AC_CLOSEf (Close a) b ->
        AClose (swap a) (pIdentToIdent b)
    AC_HALTf (Halt a) b -> 
        AHalt (swap a) (pIdentToIdent b)

    AC_PROD as -> AProd (map pIdentToIdent as)
    AC_PRODELEM a b -> AProdElem (pIdentToIdent b) (pIntegerToWord a)

    AC_EMSG str ->
        AErrMsg str
        
translateLABELCOMStoALabelComs :: LABELCOMS -> ALabelComs
translateLABELCOMStoALabelComs n = case n of
    Labelcoms1 a b (Prog cs) ->
        ALabelComs (uIdentToIdent a, uIdentToIdent b) (map translateCOMToACom cs)
    Labelcoms2 a b cs (Prog ds) ->
        ALabelComsArgs 
            ((uIdentToIdent a, uIdentToIdent b), map pIdentToIdent cs) 
            (map translateCOMToACom ds)

