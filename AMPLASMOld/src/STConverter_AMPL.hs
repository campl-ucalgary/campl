module STConverter_AMPL where

import qualified TypesAMPL as T
import qualified TypesAMPL 
import Language.AbsAMPLGrammar
import Data.Char

transPIdent :: PIdent -> T.NamePnPair
transPIdent x = case x of
  PIdent (posn,x)  -> (x,posn) 

transUIdent :: UIdent -> T.NamePnPair
transUIdent x = case x of
  UIdent (posn,x)  -> (x,posn) 

transCharacter :: Character -> (Char,T.PosnPair)
transCharacter x = case x of
  Character (posn,x) -> (read x::Char,posn) 

transPInteger :: PInteger -> (Int,T.PosnPair)
transPInteger x = case x of
  PInteger (posn,x) -> (read x::Int,posn) 

--------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------

transAMPL_CONSTRUCTS x = case x of
  HANDLE_CONSTRUCT handles  -> T.HAND_SPEC (transHANDLES handles)
  COHANDLE_CONSTRUCT cohandles -> T.COHAND_SPEC (transCOHANDLES cohandles)
  CONSTRUCTOR_CONSTRUCT constructors -> T.CONS_SPEC (transCONSTRUCTORS constructors)
  DESTRUCTOR_CONSTRUCT destructors  -> T.DEST_SPEC (transDESTRUCTORS destructors)
  PROCESSES_CONSTRUCT processes  -> T.PROC_SPEC (transPROCESSES processes)
  FUNCTIONS_CONSTRUCT functions  -> T.FUNC_SPEC (transFUNCTIONS functions)
  IMPORT_CONSTRUCT imports  -> T.IMPORT_SPEC 

transAMPLCODE :: Language.AbsAMPLGrammar.AMPLCODE -> TypesAMPL.AMPLCODE
transAMPLCODE (Main ampl_constructss start) =  T.AMPLcode handles cohandles constructors 
                                                          destructors processes functions
                                                          start1
               where  
                construct_maps = map transAMPL_CONSTRUCTS ampl_constructss 
                handles        = get_Handles construct_maps
                cohandles      = get_Cohandles construct_maps
                constructors   = get_Cons construct_maps
                destructors    = get_Dest construct_maps
                processes      = get_Proc construct_maps
                functions      = get_Func construct_maps
                start1         = transSTART start 
                --(_,com)        = start1 

get_Handles [] = []
get_Handles ((T.HAND_SPEC handle):rest) = handle ++ (get_Handles rest)
get_Handles (_:rest) = get_Handles rest 

get_Cohandles [] = []
get_Cohandles ((T.COHAND_SPEC cohandle):rest) = cohandle ++ (get_Cohandles rest)
get_Cohandles (_:rest) = get_Cohandles rest 

get_Cons [] = []
get_Cons ((T.CONS_SPEC cons):rest) = cons ++ (get_Cons rest)
get_Cons (_:rest)  = get_Cons rest 

get_Dest [] = []
get_Dest ((T.DEST_SPEC dest):rest) = dest ++ (get_Dest rest)
get_Dest (_:rest) = (get_Dest rest)

get_Proc [] = []
get_Proc ((T.PROC_SPEC proc):rest) = proc ++ (get_Proc rest)
get_Proc (_:rest) = get_Proc rest

get_Func [] = []
get_Func ((T.FUNC_SPEC func):rest)  = func ++ (get_Func rest)
get_Func (_:rest) = get_Func rest


transHANDLE_SPEC x = case x of
  Hand_spec uident handles  -> T.Handle_spec (transUIdent uident) (map transHandle handles)

transHandle x = case x of
  HandName uident  -> (transUIdent uident)


transCONSTRUCTORS x = case x of
  Constructors structor_specs  -> map transSTRUCTOR_SPEC structor_specs


transDESTRUCTORS x = case x of
  Destructors structor_specs  -> map transSTRUCTOR_SPEC structor_specs


transSTRUCTOR_SPEC x = case x of
  Struct_spec uident structs  -> T.Struct_spec (transUIdent uident) (map transSTRUCT structs)


transSTRUCT x = case x of
  Struct uident pn  -> (transUIdent uident , transPInteger pn)

transHANDLES x = case x of
  Handles handle_specs  -> map transHANDLE_SPEC handle_specs


transCOHANDLES x = case x of
  Cohandles handle_specs  -> map transHANDLE_SPEC handle_specs


transPROCESSES x = case x of
  Processes process_specs  -> map transPROCESS_SPEC   process_specs


transPROCESS_SPEC x = case x of
  Process_spec ident varss ids1 ids2 coms3  -> T.Process_specf (transPIdent ident)
                                                               (map transVars varss)
                                                               ( 
                                                                 map transPIdent ids1,
                                                                 map transPIdent ids2
                                                                ) 
                                                                ( transCOMS coms3 )

transVars x = case x of
  VName id  -> (transPIdent id)


transFUNCTIONS x = case x of
  Functions function_specs  -> map transFUNCTION_SPEC function_specs


transFUNCTION_SPEC x = case x of
  Function_spec ident varss coms  -> T.Function_specf (transPIdent ident)
                                                      (map transVars varss)
                                                      (transCOMS coms)


transSTART x = case x of
  Start (Main_run (posn,_)) channel_spec coms ->
                             ( posn,
                               (transCHANNEL_SPEC channel_spec),
                               (transCOMS coms)
                              )
  Start_none  -> ((0,0),(T.Channel_specf [] []),[])



transCHANNEL_SPEC x = case x of
  Channel_specf ids1 ids2  -> T.Channel_specf (map transPIdent ids1)
                                              (map transPIdent ids2)
  Channel_spec cintegers1 cintegers2  -> T.Channel_spec (map transCInteger cintegers1)
                                                        (map transCInteger cintegers1)


transCOMS x = case x of
  Prog coms  -> (map transCOM coms)



checkCOM com 
     | elem com' commlist = com 
     | otherwise          = error $ "Error : Can't Assign command " ++ show com 

    where 
         commlist = ["AC_CALLf","AC_GETf","AC_ADD","AC_STRUCT","AC_SUB",
                     "AC_MUL", "AC_LEQ" ,"AC_EQ","AC_EQC" ,"AC_LEQC",
                     "AC_EQCf" ,"AC_DIVQ","AC_DIVR",
                     "AC_INT", "AC_CHAR","AC_STRING","AC_UNSTRING","AC_EQS","AC_CONCATf"
                     ,"AC_CONCAT","AC_RECORDf","AC_DESTl","AC_DESTlas"]
         com'    = (head.words.show) com


transCOM x = case x of
  AC_ASSIGN ident com                   -> T.AC_ASSIGN posn nmpn ( checkCOM $ transCOM com)
                                             where 
                                               nmpn@(name,posn) = transPIdent ident
  AC_STOREf (Store(posn,_)) id          -> T.AC_STOREf posn (transPIdent id)
  AC_LOADf  (Load (posn,_)) id          -> T.AC_LOADf  posn (transPIdent id)
  AC_RET    (Ret  (posn,_))             -> T.AC_RET posn 
  AC_CALLf  (Call (posn,_)) id ids      -> T.AC_CALLf posn (transPIdent id)
                                                           (map transPIdent ids)
  AC_INT    (ConstInt (posn,_)) cint    -> T.AC_INT posn    (transCInteger cint)
  AC_CHAR   (ConstChar(posn,_)) char    -> T.AC_CHAR posn   (transCharacter char)
  AC_STRING (ConstString (posn,_)) ustr -> T.AC_STRING posn  (ustr,(0,0)) --(transCString ustr)  -- string functions start
  AC_UNSTRING (Unstring (posn,_))       -> T.AC_UNSTRING posn 
  AC_EQS    (Eqs(posn,_))               -> T.AC_EQS posn 
  AC_LEQS   (Leqs(posn,_))              -> T.AC_LEQS posn

  AC_TOINT    (ToInt(posn,_))           -> T.AC_TOINT posn 
  AC_TOSTR    (ToStr(posn,_))           -> T.AC_TOSTR posn  

  AC_AND    (And(posn,_))              -> T.AC_AND posn 
  AC_OR     (Or (posn,_))              -> T.AC_OR posn  

  AC_APPEND (Append(posn,_))            -> T.AC_APPEND posn  

  AC_CONCAT (ConcatS(posn,_)) n         -> T.AC_CONCAT posn (fromInteger n)  
  AC_LEQ    (LeqI(posn,_))              -> T.AC_LEQ posn 
  AC_EQ     (EqI (posn,_))              -> T.AC_EQ posn 
  AC_LEQC   (Leqc(posn,_))              -> T.AC_LEQC posn 
  AC_EQC    (Eqc (posn,_))              -> T.AC_EQC posn 
  AC_ADD    (Add (posn,_))              -> T.AC_ADD posn 
  AC_SUB    (Subtract (posn,_))         -> T.AC_SUB posn 
  AC_DIVQ   (Quot (posn,_))             -> T.AC_DIVQ posn 
  AC_DIVR   (Rem  (posn,_))             -> T.AC_DIVR posn 
  AC_MUL    (Mul  (posn,_))             -> T.AC_MUL posn 
  AC_STRUCT uident1 uident2             -> T.AC_STRUCT (
                                                       transUIdent uident1,
                                                       transUIdent uident2
                                                       )
                                                       []
  AC_STRUCTas uident1 uident2 idents    -> T.AC_STRUCT (
                                                         transUIdent uident1,
                                                         transUIdent uident2
                                                       ) 
                                                       ( map transPIdent idents )

  AC_CASEf   (Case (posn,_)) labelcomss -> T.AC_CASEf posn ( map transLABELCOMS labelcomss )
  AC_RECORDf (Rec  (posn,_)) labelcomss -> T.AC_RECORDf posn (map transLABELCOMS labelcomss)
  AC_GETf   (Get  (posn,_)) id1 id2     -> T.AC_GETf posn (transPIdent id1)
                                                          (transPIdent id2)
  AC_HPUTf  (Hput (posn,_)) id uident1 
                               uident2  -> T.AC_HPUTf posn  (transPIdent id)
                                                            (
                                                              transUIdent uident1,
                                                              transUIdent uident2
                                                            )

  AC_HCASEf (Hcase(posn,_)) id 
                            labelcomss  -> T.AC_HCASEf posn (transPIdent id)
                                                            double_list
                                                where 
                                                  triple_list = map transLABELCOMS labelcomss
                                                  double_list = map (\(x,y,z) -> (x,z)) triple_list

  AC_PUTf   (Put   (posn,_)) id         -> T.AC_PUTf posn (transPIdent id)
  AC_SPLITf (Split(posn,_)) id1 id2 id3 -> T.AC_SPLITf posn  (transPIdent id1)
                                                                (
                                                                  transPIdent id2,
                                                                  transPIdent id3
                                                                )
  AC_FORKf  (Fork (posn,_)) id1 id2  
              ids3 coms4 id5 ids6 coms7 -> T.AC_FORKf posn (transPIdent id1) (c1,c2)
                                                    where c1 = (
                                                                 transPIdent id2,
                                                                 map transPIdent ids3,
                                                                 transCOMS coms4
                                                                )
                                                          c2 = ( 
                                                                 transPIdent id5,
                                                                 map transPIdent ids6,
                                                                 transCOMS coms7
                                                                )
  AC_PLUGf  (Plug (posn,_)) idents ids1 
            coms2 ids3 coms4            -> T.AC_PLUGf posn (map transPIdent idents)
                                                           (map transPIdent ids1,transCOMS coms2)
                                                           (map transPIdent ids3,transCOMS coms4)

  AC_RUNf   (Run (posn,_)) ident ids1 
                           ids2 ids3    -> T.AC_RUNf posn  ( transPIdent ident)
                                                           ( map transPIdent ids1)
                                                           ( map transPIdent ids2,
                                                             map transPIdent ids3 
                                                           ) 
  AC_CLOSEf (Close (posn,_)) id         -> T.AC_CLOSEf posn (transPIdent id)
  AC_HALTf  (Halt  (posn,_)) ids        -> T.AC_HALTf posn (map transPIdent ids)
  AC_IDF    id1 (Ch_Id (posn,_)) id2    -> T.AC_IDf posn (transPIdent id1) (transPIdent id2)
  AC_PROD pidents                       -> T.AC_PROD (map transPIdent pidents)
  AC_PRODELEM cint pident               -> T.AC_PRODELEM posnp intp (transPIdent pident)
                                              where
                                                 (intp,posnp) = transCInteger cint 
  
  AC_EMSG emsg                          -> T.AC_EMSG emsg 


transLABELCOMS x = case x of
  Labelcoms1 uident1 uident2 coms3  -> ( (
                                           transUIdent uident1,
                                           transUIdent uident2
                                         ),
                                         [],
                                         transCOMS coms3
                                        )
  Labelcoms2 uident1 uident2 idents coms3  -> (
                                                ( transUIdent uident1,
                                                  transUIdent uident2
                                                ),
                                                map transPIdent idents,
                                                transCOMS coms3
                                              )




transCInteger x = case x of
  Positive n  -> (transPInteger n)
  Negative n  -> (transPInteger n)


                    
