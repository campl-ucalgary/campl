{-# LANGUAGE TupleSections #-}
module AMPLUntaggedConstructBag where

import Language.ParAMPL
import Language.LexAMPL
import Language.AbsAMPL
import Language.ErrM
import Language.LayoutAMPL

import AMPLAST

import Data.Data
import Data.Tuple
import Data.Maybe
import Data.Coerce
import Data.Typeable
import Text.Read ( readMaybe )

import AMPLTypes
import Data.Stream (Stream)
import qualified Data.Stream as Stream

{-
    This includes untagged construct bags..
    Translate to this, if we are translating from a different representation
    since the progream should be semantically valid. 

    This defines a translation to the bnfc printer, which will be parsed
    and use the rest of the assemblers normal facilities...
-}

type UntaggedACom = AComIdents () String

type UntaggedProtocolInfo = [String]
    -- handles
type UntaggedCoprotocolInfo = [String]
    -- cohandles
type UntaggedDataInfo = [(String, Word)]
    -- constructors, num args
type UntaggedCodataInfo = [(String, Word)]
    -- destructors, num args
type UntaggedProcessInfo = ([String], [String], [String], [UntaggedACom])
    -- seqvars, inchsw, outchs, cmsd
type UntaggedFunctionInfo = ([String], [UntaggedACom])
    -- args, commands

type UntaggedMainInfo = Maybe ([String], [String], [UntaggedACom])
        -- ^  ((input channels, output channels), instructions)
        -- Note that name will always be %run when parsed by BNFC


data UntaggedAmplAsmBag = UntaggedAmplAsmBag {
    untaggedAmplImports :: [FilePath]
    , untaggedAmplMainInfo :: UntaggedMainInfo
    , untaggedAmplConstructsBag :: UntaggedAmplConstructsBag
}
        -- ^ (imports info, MainInfo, AmplConstructsBag)

-- | A bag of all the constructs in a program
data UntaggedAmplConstructsBag = UntaggedAmplConstructsBag {
    untaggedProtocolInfo :: [(String, UntaggedProtocolInfo)]
    , untaggedCoprotocolInfo :: [(String, UntaggedCoprotocolInfo)]
    , untaggedDataInfo :: [(String, UntaggedDataInfo)]
    , untaggedCodataInfo :: [(String, UntaggedCodataInfo)]
    , untaggedProcessInfo :: [(String, UntaggedProcessInfo)]
    , untaggedFunctionInfo :: [(String, UntaggedFunctionInfo)]
    } deriving (Show, Eq)

type UntaggedALabelComs = ALabelComsIdents () String


nullPos :: (Int, Int)
nullPos = (-1, -1)

untaggedAmplAsmBagToAMPLCODE :: 
    UntaggedAmplAsmBag -> 
    AMPLCODE 
untaggedAmplAsmBagToAMPLCODE 
    UntaggedAmplAsmBag{ untaggedAmplImports = imports, untaggedAmplMainInfo = maincts, untaggedAmplConstructsBag =  bag} =
        Main constructs start 
  where
    constructs = importscts ++ [handlescts,cohandlescts,codatacts,functionscts,processescts]

    importscts = map (IMPORT_CONSTRUCT . Import . coerce . (nullPos,)) imports
    handlescts = HANDLE_CONSTRUCT $ Handles $ map handleshelper $ untaggedProtocolInfo bag
    cohandlescts = COHANDLE_CONSTRUCT $ Cohandles $ map handleshelper $ untaggedCoprotocolInfo bag

    datacts = CONSTRUCTOR_CONSTRUCT $ Constructors $ map constructorshelper $ untaggedDataInfo bag
    codatacts = DESTRUCTOR_CONSTRUCT $ Destructors $ map constructorshelper $ untaggedCodataInfo bag

    functionscts = FUNCTIONS_CONSTRUCT $ Functions $ map functionshelper $ untaggedFunctionInfo bag
    processescts = PROCESSES_CONSTRUCT $ Processes $ map processeshelper $ untaggedProcessInfo bag

    handleshelper :: (String, UntaggedProtocolInfo) -> HANDLE_SPEC
    handleshelper (name, subs) = 
        Hand_spec 
            (coerce (nullPos,name)) 
            (map (HandName . coerce . (nullPos,)) subs)

    constructorshelper :: (String, [(String, Word)]) -> STRUCTOR_SPEC
    constructorshelper (name, subs) = 
        Struct_spec 
            (coerce (nullPos, name)) 
            (map ((\(ident, args) -> Struct 
                    (coerce $ (nullPos,) ident) 
                    (PInteger $ (nullPos, show args)) )) subs
                    )

    functionshelper :: (String, UntaggedFunctionInfo) -> FUNCTION_SPEC
    functionshelper (name, (args, cmds)) = 
        Function_spec
            (coerce (nullPos, name))
            (map (VName . coerce . (nullPos,)) args)
            (Prog $ map translateUntaggedAComToCOM cmds)

    processeshelper :: (String, UntaggedProcessInfo) -> PROCESS_SPEC
    processeshelper (name, (seqvars, inchs, outchs, cmds)) = 
        Process_spec
            (coerce (nullPos, name))
            (map (VName . coerce . (nullPos,)) seqvars)
            (map (coerce . (nullPos,)) inchs)
            (map (coerce . (nullPos,)) outchs)
            (Prog $ map translateUntaggedAComToCOM cmds)

    start = case maincts of
            Just (inchs, outchs, cmds) -> 
                Start 
                    (Main_run (nullPos, "%run")) 
                    (Channel_spec 
                        (map (coerce . (nullPos,)) inchs) 
                        (map (coerce . (nullPos,)) outchs))
                    (Prog $ map translateUntaggedAComToCOM cmds)
            Nothing ->  Start_none


translateUntaggedAComToCOM :: UntaggedACom -> COM 
translateUntaggedAComToCOM n = case n of
    AAssign a b -> AC_ASSIGN (coerce $ (nullPos,) a) (translateUntaggedAComToCOM b)
    AStore a b -> AC_STOREf (Store (nullPos, "store")) (coerce $ (nullPos,) b)
    ALoad a b -> AC_LOADf (Load (nullPos, "load")) (coerce $ (nullPos,) b)
    ARet a -> AC_RET (Ret (nullPos,"ret")) 
    ACall a b cs -> 
        AC_CALLf (Call (nullPos, "call")) (coerce $ (nullPos,) b) (map (coerce . (nullPos,)) cs)
    -- uh oh! not a true isomorphism -- but it should be okay...
    AInt a v -> 
        AC_INT 
            (ConstInt (nullPos,"cInt")) 
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
        in AC_CHAR (ConstChar (nullPos, "cChar")) (Character ((-1,-1), v'))
        
    AString a str -> AC_STRING (ConstString (nullPos, "cString")) str 

    AToStr a -> AC_TOSTR (ToStr (nullPos, "toStr")) 
    AToInt a -> AC_TOINT (ToInt (nullPos, "toInt")) 
        
    AAnd a -> AC_AND (And $ (nullPos, "and")) 
    AOr a -> AC_OR (Or $ (nullPos, "or")) 
    AAppend a -> AC_APPEND (Append (nullPos, "append")) 
    ATrue a -> AC_TRUE (BTrue (nullPos, "true")) 
    AFalse a -> AC_FALSE (BFalse (nullPos, "false"))

    {-
    AC_UNSTRING (Unstring a) ->
        error "no AC_UNSTRING instance"
    -}
    ALeqInt a -> AC_LEQ (LeqI (nullPos, "leq")) 
    AEqInt a -> AC_EQ (EqI (nullPos, "eq")) 
    ALeqChar a -> AC_LEQC (Leqc (nullPos, "leqc")) 
    AEqChar a -> AC_EQC (Eqc (nullPos, "eqc")) 

    {-
    AC_LEQS (Leqs a) ->
        error "no AC_LEQS instance"
    AC_EQS (Eqs a) -> 
        error "no AC_EQS instance"
    AC_CONCAT (ConcatS a) b ->
        error "no AC_CONCAT instance"
    -}
        
    AAddInt a -> AC_ADD (Add (nullPos, "add")) 
        
    ASubInt a -> AC_SUB (Subtract (nullPos, "subtract")) 
    AMulInt a -> AC_MUL (Mul (nullPos, "mul")) 
    AModInt a -> AC_DIVQ (Quot (nullPos, "quot")) 
    ADivInt a -> AC_DIVR (Rem (nullPos, "rem")) 

    {-
    AC_CONS (Cons a) b c ->
        error "no AC_CONS instance"
    -}

    AConstructor (a,b) -> AC_STRUCT (coerce (nullPos, a)) (coerce (nullPos, b))
    AConstructorArgs ((a, b), args) ->
        AC_STRUCTAS (coerce (nullPos, a)) (coerce $ (nullPos, b)) (map (coerce . (nullPos,)) args) 

    ACase a (b, cs) ->
        AC_CASEf (Case $ (nullPos, "case")) (coerce $ (nullPos,) b) (map translateUntaggedALabelComsToLABELCOMS cs) 
    AIf a (b , (cs , ds)) -> 
        AC_IF (If $ (nullPos, "if")) (coerce $ (nullPos, b)) (Prog $ map translateUntaggedAComToCOM cs) (Prog $ map translateUntaggedAComToCOM ds) 
    ARecord a bs ->
        AC_RECORDf (Rec $ coerce (nullPos, "rec") ) (map translateUntaggedALabelComsToLABELCOMS bs) 

    ADest (a,b) c -> 
        AC_DEST (coerce $ (nullPos,) a) (coerce $ (nullPos,) b) (coerce $ (nullPos,) c)
    ADestArgs ((a, b), cs) d -> 
        AC_DESTAS (coerce $ (nullPos,) a) (coerce $ (nullPos,) b) (map (coerce . (nullPos,)) cs) (coerce $ (nullPos,) d)

    AGet a b c -> 
        AC_GETf (Get $ (nullPos, "get")) (coerce $ (nullPos,) b) (coerce $ (nullPos,) c) 
    AHPut a (b,c) d ->
        AC_HPUTf (Hput $ (nullPos, "hput")) (coerce $ (nullPos,) b) (coerce $ (nullPos,) c) (coerce $ (nullPos,) d) 

    AHCase a (b, cs) -> 
        AC_HCASEf (Hcase $ (nullPos, "hcase")) (coerce $ (nullPos,) b) (map translateUntaggedALabelComsToLABELCOMS cs)
    APut a b c -> 
        AC_PUTf (Put $ (nullPos, "put")) (coerce $ (nullPos,) b) (coerce $ (nullPos,) c) 

    ASplit a b c d ->
        AC_SPLITf (Split $ (nullPos, "split")) (coerce $ (nullPos,) b) (coerce $ (nullPos,) c) (coerce $ (nullPos,) d) 


    AFork a b ((a1, a1s), as) ((b1, b1s), bs) ->
        AC_FORKf
            (Fork $ (nullPos, "fork")) (coerce $ (nullPos,) b) 
            (coerce $ (nullPos,) a1) 
            (map (coerce . (nullPos,)) a1s) (Prog $ map translateUntaggedAComToCOM as) 
            (coerce $ (nullPos,) b1) (map (coerce . (nullPos,)) b1s) (Prog $ map translateUntaggedAComToCOM bs)  

    APlug a as (a1s, a2s) (b1s, b2s) -> 
        AC_PLUGf (Plug $ (nullPos, "plug") ) (map (coerce . (nullPos,)) as) 
            (map (coerce . (nullPos,)) a1s) (Prog $ map translateUntaggedAComToCOM a2s) 
            (map (coerce . (nullPos,))  b1s) (Prog $ map translateUntaggedAComToCOM b2s) 

    ARun a b (cs, (ds, es)) ->
        AC_RUNf (Run $ (nullPos,"run")) (coerce $ (nullPos,) b) (map (coerce . (nullPos,)) cs) (map (coerce . (nullPos,)) ds) (map (coerce . (nullPos,)) es) 

    AId b (a,c) -> 
        AC_IDF (coerce $ (nullPos,) a) (Ch_Id $ coerce $ (nullPos, "==")) (coerce $ (nullPos,) c) 

    ARace a bs -> 
        AC_RACE (Race (nullPos, "race")) (map f bs) 
      where
        f :: (String, [AComIdents () String]) -> RACES
        f (a, coms) = (Races (coerce $ (nullPos,) a) (Prog $ map translateUntaggedAComToCOM coms)) 

    AClose a b -> 
        AC_CLOSEf (Close $ (nullPos,"close")) (coerce (nullPos, b)) 
    AHalt a b -> 
        AC_HALTf (Halt $ (nullPos, "halt")) (coerce (nullPos, b)) 

    {-
    AC_PROD as -> AProd (map pIdentToIdent as)
    AC_PRODELEM a b -> AProdElem (pIdentToIdent b) (pIntegerToWord a)

    AC_EMSG str ->
        AErrMsg str
        
-}

translateUntaggedALabelComsToLABELCOMS :: ALabelComsIdents () String -> LABELCOMS 
translateUntaggedALabelComsToLABELCOMS n = case n of
    ALabelComs (a,b) cs -> 
        Labelcoms1 (coerce $ (nullPos,) a) (coerce $ (nullPos,) b) (Prog $ map translateUntaggedAComToCOM cs) 
        
    ALabelComsArgs ((a,b),  cs)  ds ->
        Labelcoms2 (coerce $ (nullPos, a)) (coerce $ (nullPos, b)) (map (coerce . (nullPos,)) cs) (Prog $ map translateUntaggedAComToCOM ds) 
