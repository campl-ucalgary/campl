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
data ACom = 
    AAssign InstrIdent ACom
    | AStore InstrIdent Ident
    | ALoad InstrIdent Ident
    | ARet InstrIdent
    | ACall InstrIdent Ident [Ident]
    | AInt InstrIdent Int
    | AChar InstrIdent Char

    | AString InstrIdent String
    | AToStr InstrIdent 
    | AToInt InstrIdent 

    | AAnd InstrIdent
    | AOr InstrIdent

    | AAppend InstrIdent

    | ATrue InstrIdent 
    | AFalse InstrIdent 

    | ALeqInt InstrIdent
    | AEqInt InstrIdent

    | AEqChar InstrIdent
    | ALeqChar InstrIdent

    | AEqString InstrIdent
    | ALeqString InstrIdent

    | AConcat InstrIdent Int

    | AAddInt InstrIdent
    | ASubInt InstrIdent
    | AMulInt InstrIdent
    | ADivInt InstrIdent
    | AModInt InstrIdent

    | AConstructor (Ident, Ident)
    | AConstructorArgs ((Ident, Ident), [Ident])

    | ACase InstrIdent (Ident, [ALabelComs])

    | AIf InstrIdent (Ident ,([ACom], [ACom]))

    | ARecord InstrIdent [ALabelComs]
    | ADest (Ident, Ident) Ident
    | ADestArgs ((Ident, Ident), [Ident]) Ident

    | AGet InstrIdent Ident Ident
    | AHPut InstrIdent (Ident, Ident) Ident
    | AHCase InstrIdent (Ident, [ALabelComs])

    | APut InstrIdent Ident Ident

    | ASplit InstrIdent Ident Ident Ident
    | AFork InstrIdent Ident 
        ((Ident, [Ident]), [ACom])
        ((Ident, [Ident]), [ACom])
    | APlug InstrIdent [Ident]
        ([Ident], [ACom])
        ([Ident], [ACom])
    | ARun InstrIdent Ident 
        ([Ident], ([Ident], [Ident]))
    | AId InstrIdent (Ident, Ident)
    | ARace InstrIdent [(Ident, [ACom])]
    | AClose InstrIdent Ident
    | AHalt InstrIdent Ident

    | AProd [Ident]
    | AProdElem InstrIdent Word

    | AErrMsg String
  deriving (Show, Eq)

data ALabelComs =
    ALabelComs (Ident, Ident) [ACom]
    | ALabelComsArgs ((Ident, Ident), [Ident]) [ACom]
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

translateAComToCOM :: ACom -> COM 
translateAComToCOM n = case n of
    AAssign a b -> AC_ASSIGN (coerce $ swap a) (translateAComToCOM b)
    AStore a b -> AC_STOREf (Store $ swap a) (coerce $ swap b)
    ALoad a b -> AC_LOADf (Load $ swap a) (coerce $ swap b)
    ARet a -> AC_RET (Ret $ swap a) 
    ACall a b cs -> 
        AC_CALLf (Call $ swap a) (coerce $ swap b) (map (coerce . swap) cs)
    -- uh oh! not a true isomorphism -- but it should be okay...
    AInt a v -> 
        AC_INT 
            (ConstInt $ swap a) 
            (if v < 0 
                then Negative (PInteger ((-1,-1), show (negate v)))
                else Positive (PInteger ((-1,-1), show v))
            )
    AChar a v -> 
        let v' = case v of
                    '\\' -> "\\\\" 
                    '\'' -> "\'" 
                    '\n' -> "\n" 
                    '\t' -> "\t" 
                    c -> [c]
        in AC_CHAR (ConstChar $ swap a) (Character ((-1,-1), v'))
        
    AString a str -> AC_STRING (ConstString $ swap a) str 

    AToStr a -> AC_TOSTR (ToStr $ swap a) 
    AToInt a -> AC_TOINT (ToInt $ swap a) 
        
    AAnd a -> AC_AND (And $ swap a) 
    AOr a -> AC_OR (Or $ swap a) 
    AAppend a -> AC_APPEND (Append $ swap a) 
    ATrue a -> AC_TRUE (BTrue $ swap a) 
    AFalse a -> AC_FALSE (BFalse $ swap a)

    {-
    AC_UNSTRING (Unstring a) ->
        error "no AC_UNSTRING instance"
    -}
    ALeqInt a -> AC_LEQ (LeqI $ swap a) 
    AEqInt a -> AC_EQ (EqI $ swap a) 
    ALeqChar a -> AC_LEQC (Leqc $ swap a) 
    AEqChar a -> AC_EQC (Eqc $ swap a) 

    {-
    AC_LEQS (Leqs a) ->
        error "no AC_LEQS instance"
    AC_EQS (Eqs a) -> 
        error "no AC_EQS instance"
    AC_CONCAT (ConcatS a) b ->
        error "no AC_CONCAT instance"
    -}
        
    AAddInt a -> AC_ADD (Add $ swap a) 
        
    ASubInt a -> AC_SUB (Subtract $ swap a) 
    AMulInt a -> AC_MUL (Mul $ swap a) 
    AModInt a -> AC_DIVQ (Quot $ swap a) 
    ADivInt a -> AC_DIVR (Rem $ swap a) 

    {-
    AC_CONS (Cons a) b c ->
        error "no AC_CONS instance"
    -}

    AConstructor (a,b) -> AC_STRUCT (coerce $ swap a) (coerce $ swap b)
    AConstructorArgs ((a, b), args) ->
        AC_STRUCTAS (coerce $ swap a) (coerce $ swap b) (map (coerce. swap) args) 

    ACase a (b,  cs) ->
        AC_CASEf (Case $ swap a) (coerce $ swap b) (map translateALabelComsToLABELCOMS cs) 
    AIf a (b , (cs , ds)) -> 
        AC_IF (If $ swap a) (coerce $ swap b) (Prog $ map translateAComToCOM cs) (Prog $ map translateAComToCOM ds) 
    ARecord a bs ->
        AC_RECORDf (Rec $ swap a) (map translateALabelComsToLABELCOMS bs) 

    ADest (a,b) c -> 
        AC_DEST (coerce $ swap a) (coerce $ swap b) (coerce $ swap c)
    ADestArgs ((a, b), cs) d -> 
        AC_DESTAS (coerce $ swap a) (coerce $ swap b) (map (coerce . swap) cs) (coerce $ swap d)

    AGet a b c -> 
        AC_GETf (Get $ swap a) (coerce $ swap b) (coerce $ swap c) 
    AHPut a (b,c) d ->
        AC_HPUTf (Hput $ swap a) (coerce $ swap b) (coerce $ swap c) (coerce $ swap d) 

    AHCase a (b, cs) -> 
        AC_HCASEf (Hcase $ swap a) (coerce $ swap b) (map translateALabelComsToLABELCOMS cs)
    APut a b c -> 
        AC_PUTf (Put $ swap a) (coerce $ swap b) (coerce $ swap c) 

    ASplit a b c d ->
        AC_SPLITf (Split $ swap a) (coerce $ swap b) (coerce $ swap c) (coerce $ swap d) 


    AFork a b ((a1, a1s), as) ((b1, b1s), bs) ->
        AC_FORKf
            (Fork $ swap a) (coerce $ swap b) 
            (coerce $ swap a1) 
            (map (coerce . swap) a1s) (Prog $ map translateAComToCOM as) 
            (coerce $ swap b1) (map (coerce . swap) b1s) (Prog $ map translateAComToCOM bs)  

    APlug a as (a1s, a2s) (b1s, b2s) -> 
        AC_PLUGf (Plug $ swap a) (map (coerce . swap) as) 
            (map (coerce . swap) a1s) (Prog $ map translateAComToCOM a2s) 
            (map (coerce . swap)  b1s) (Prog $ map translateAComToCOM b2s) 

    ARun a b (cs, (ds, es)) ->
        AC_RUNf (Run $ swap a) (coerce $ swap b) (map (coerce . swap) cs) (map (coerce . swap) ds) (map (coerce . swap) es) 

    AId b (a,c) -> 
        AC_IDF (coerce $ swap a) (Ch_Id $ swap b) (coerce $ swap c) 

    ARace a bs -> 
        AC_RACE (Race $ swap a) (map f bs) 
      where
        f :: (Ident, [ACom]) -> RACES
        f (a, coms) = (Races (coerce $ swap a) (Prog $ map translateAComToCOM coms)) 

    AClose a b -> 
        AC_CLOSEf (Close $ swap a) (coerce $ swap b) 
    AHalt a b -> 
        AC_HALTf (Halt $ swap a) (coerce $ swap b) 

    {-
    AC_PROD as -> AProd (map pIdentToIdent as)
    AC_PRODELEM a b -> AProdElem (pIdentToIdent b) (pIntegerToWord a)

    AC_EMSG str ->
        AErrMsg str
        
-}

translateALabelComsToLABELCOMS :: ALabelComs -> LABELCOMS 
translateALabelComsToLABELCOMS n = case n of
    ALabelComs (a,b) cs -> 
        Labelcoms1 (coerce $ swap a) (coerce $ swap b) (Prog $ map translateAComToCOM cs) 
        
    ALabelComsArgs ((a,b),  cs)  ds ->
        Labelcoms2 (coerce $ swap a) (coerce $ swap b) (map (coerce . swap) cs) (Prog $ map translateAComToCOM ds) 
