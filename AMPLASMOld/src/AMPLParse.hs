{-# LANGUAGE LambdaCase #-}
module AMPLParse where

import STConverter_AMPL
import TypesAMPL hiding (Translation)
import CMPL.CompileAll

import AMPLTypes
import AMPLServices

import Language.ParAMPLGrammar
import Language.LexAMPLGrammar
import Language.AbsAMPLGrammar
import Language.ErrM
import Language.LayoutAMPLGrammar


-- pAMPLCODE (parses the tokens)
-- resolveLayout True . myLexer 

-- | Throws exceptions..
parseAMPL = 
    compile_all
    . transAMPLCODE
    . (\case Bad s -> error s ; Ok a -> a)
    . stringToAMPLCODE

machToAmplMach :: 
    MACH -> 
    ( ([GlobalChanID], [(GlobalChanID, (ServiceDataType, ServiceType))]) 
        -- used for  genServicesChmAndStream
    , ([Instr], [Translation]) 
        -- main function and translations
    , [(FunID, (String, [Instr]))] 
        -- list of functions (and protocols)
    )
machToAmplMach ([([],ts,[], amplcoms)], (_,chs), defs) = undefined
machToAmplMach _ = error "Invalid initial machine passed."

amplComsToInstrs :: [AMPLCOM] -> [Instr]
amplComsToInstrs = map f
  where
    f (AMC_GET ch) = iGet (LocalChanID ch)
    f (AMC_HPUT ch n) 
        | n < 0 =  error ("AMC_HPUT negative value: " ++ show n ++ " on channel " ++ show ch)
        | otherwise = iHPut (LocalChanID ch) (HCaseIx (fromIntegral n))
        -- NOTE -- the fromIntegral here is because when the system was originally designed,
        -- we assumed all array indices will be positive. CHECK THIS TO BE SURE...
    f (AMC_HCASE ch lstinstrs) = iHCase (LocalChanID ch) (map amplComsToInstrs lstinstrs)
    f (AMC_hcase lstinstrs) = iCase (map amplComsToInstrs lstinstrs)
    f (AMC_PUT ch) = iPut (LocalChanID ch)
    f (AMC_SPLIT ch (ch1,ch2)) = iSplit (LocalChanID ch) (LocalChanID ch1, LocalChanID ch2)

        {-
        AMC_FORK CH ((CH,[CH],AMPLCOMS),(CH,[CH],AMPLCOMS))
        AMC_PLUG [CH] ([CH],AMPLCOMS)  ([CH],AMPLCOMS)
        AMC_CLOSE CH
        AMC_HALT [CH]
        AMC_RUN TRANS String Int
        AMC_RACE [(CH, AMPLCOMS)]

        AMC_LOAD Int               -- access
        AMC_CALL String Int
        AMC_STORE 
        AMC_RET
        AMC_PRET

        AMC_STRING String -- string data types start
        AMC_UNSTRING 
        AMC_EQS
        AMC_LEQS
        AMC_CONCAT
        AMC_CONCATf Int -- string data types end
        AMC_TOSTR
        AMC_TOINT
        AMC_APPEND

        AMC_OR
        AMC_AND
 
        AMC_INT Int -- Int data types start
        AMC_LEQ 
        AMC_EQ   
        AMC_ADD
        AMC_SUB 
        AMC_MUL 
        AMC_DIVQ
        AMC_DIVR -- Int data types end

        AMC_CHAR Char -- Char data types start
        AMC_LEQC 
        AMC_EQC  -- char data types end

        AMC_CONS Int Int 
        AMC_CASE [AMPLCOMS] 
        AMC_REC [AMPLCOMS]
        AMC_DEST Int Int 
        AMC_ID CH CH     
        AMC_PROD Int -- no of elements in the product
        AMC_PRODELEM Int -- here int is the element number of the tuple 
        AMC_ERROR String   
        -}

stringToAMPLCODE :: String -> Err Language.AbsAMPLGrammar.AMPLCODE
stringToAMPLCODE = 
    Language.ParAMPLGrammar.pAMPLCODE 
    . Language.LayoutAMPLGrammar.resolveLayout True 
    . Language.ParAMPLGrammar.myLexer


