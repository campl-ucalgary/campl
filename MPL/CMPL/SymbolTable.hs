module CMPL.SymbolTable where

-- import AMPL.TypesAMPL

import Data.List
import qualified  Data.Map as M 

import Control.Monad.State


--------------------------------------------------------------------------------------
--  Global Symbol table 
--------------------------------------------------------------------------------------


data SYM_ENTRY =  S_HAND   [(String,Int)]
                | S_CHAND  [(String,Int)]
                | S_CONSTR [(String,Int,Int)] -- Number of args is the last one
                | S_DESTR  [(String,Int,Int)]
                | S_FUN Int
                | S_PROC Int (Int,Int)
             deriving (Eq,Show)

type SYM_TBL = M.Map String SYM_ENTRY

data SYM_VALUES =  SYM_CONSTR Int Int
                 | SYM_DESTR Int Int
                 | SYM_HANDLE Int
                 | SYM_COHANDLE Int
                 | SYM_FUN Int
                 | SYM_PROCESS Int (Int,Int)
             deriving (Eq,Show)


collect_symbols:: AMPLCODE  -> SYM_TBL
collect_symbols ( AMPLcode hs chs cs ds ps fs _ ) = M.fromList st
     where
      st = (collect_functions fs.collect_processes ps.
            collect_destructors ds.collect_constructors cs.
            collect_cohandles chs.collect_handles hs) []
--------------------------------------------------------------------------------------
-----------------------HELPER FUNCTIONS FOR collect_symbols---------------------------
nums = [1..]

getNames :: [NamePnPair] -> [Name] 
getNames nposns = map (\(n,_) -> n) nposns  

--------------------------------------------------------------------------------------

collect_handles ::[HANDLE_SPEC] -> [(String,SYM_ENTRY)] -> [(String,SYM_ENTRY)]
collect_handles hs   = (++) (map helper_handles hs) 
       where
        helper_handles :: HANDLE_SPEC -> (String,SYM_ENTRY)
        helper_handles (Handle_spec (pname,_) hs) = ( 
                                                      pname,
                                                      S_HAND (zip (getNames hs) nums)
                                                    ) 

--------------------------------------------------------------------------------------

collect_cohandles ::[HANDLE_SPEC] -> [(String,SYM_ENTRY)] -> [(String,SYM_ENTRY)]
collect_cohandles chs  = (++) (map helper_cohandles chs)  
       where
        helper_cohandles :: HANDLE_SPEC -> (String,SYM_ENTRY)
        helper_cohandles (Handle_spec (pname,_) hs) = (
                                                        pname,
                                                        S_CHAND (zip (getNames hs) nums)
                                                      ) 

--------------------------------------------------------------------------------------

collect_constructors ::[STRUCTOR_SPEC] -> [(String,SYM_ENTRY)] -> [(String,SYM_ENTRY)]
collect_constructors cs = (++) (map helper_constructors cs)
       where
        helper_constructors :: STRUCTOR_SPEC -> (String,SYM_ENTRY)
        helper_constructors (Struct_spec (dname,_) hs)
                        = ( dname,
                            S_CONSTR (zipWith (\(s,m) n -> (s,n,m)) (getStructors hs) nums)
                          )

--------------------------------------------------------------------------------------

collect_destructors ::[STRUCTOR_SPEC] ->[(String,SYM_ENTRY)] -> [(String,SYM_ENTRY)]
collect_destructors ds = (++) (map helper_destructors ds)
       where
        helper_destructors :: STRUCTOR_SPEC -> (String,SYM_ENTRY)
        helper_destructors (Struct_spec (cdname,_) hs)
                        = ( cdname,
                            S_DESTR (zipWith (\(s,m) n -> (s,n,m)) (getStructors hs) nums)
                          )

getStructors :: [(NamePnPair,(Int,PosnPair))] -> [(String,Int)]
getStructors list = map (\((nm,_),(int,_)) -> (nm,int)) list 


--------------------------------------------------------------------------------------

collect_functions ::[FUNCTION_SPEC] -> [(String,SYM_ENTRY)] -> [(String,SYM_ENTRY)]
collect_functions fs = (++) (map helper_functions fs)
       where
          helper_functions :: FUNCTION_SPEC -> (String,SYM_ENTRY)
          helper_functions (Function_specf (fname,_) vs  _) = (fname,S_FUN (length vs))
          helper_functions (Function_spec  (fname,_) _)     = (fname,S_FUN 0)

--------------------------------------------------------------------------------------

collect_processes ::[PROCESS_SPEC] -> [(String,SYM_ENTRY)] ->  [(String,SYM_ENTRY)]
collect_processes ps =(++) (map helper_processes ps)
       where
          helper_processes :: PROCESS_SPEC -> (String,SYM_ENTRY)
          helper_processes (Process_specf (pname,_) vs (ins,outs) _) 
                   = ( pname,S_PROC (length vs)
                                    (length ins,length outs)
                     )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Symbol table look up facilities
--------------------------------------------------------------------------------
inlist' ::(Eq a) =>  a -> [a] -> Bool
inlist' item = foldl (\acc x -> acc || (item == x) ) False 

maybeValToVal :: Maybe a -> String -> a
maybeValToVal val message = case val of 
         Just x  -> x 
         Nothing -> error message

look_up_sym_posn :: SYM_TBL -> NamePnPair -> ErrorMsg ->  SYM_VALUES
look_up_sym_posn sym_tab (str,posn) gerror = look_up_sym_helper val'         
    where
      val  = M.lookup str sym_tab
      msg  = " Symbol < "++ str ++ " > not in scope."
      val' = maybeValToVal val (gerror ++ equals ++ genLnNoError posn ++ msg ++ equals) 
      look_up_sym_helper :: SYM_ENTRY -> SYM_VALUES
      look_up_sym_helper entry = case entry of 
          S_FUN n     -> SYM_FUN n
          S_PROC n pq -> SYM_PROCESS n pq
          _           -> error $ gerror ++ equals ++
                                 genLnNoError posn ++
                                 " Symbol <" ++ str ++
                                 "> is not a function or process." ++ equals

{-
               gerror ++ equals ++
               "Following handles have been hcased on " ++ show n ++ " times."
               ++ errors ++ equals    
-}
--------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------
-- get all structors of a data/codata  with how many arguments they take 
get_all_structors :: SYM_TBL -> NamePnPair -> ErrorMsg -> [(Name,Int)]
get_all_structors sym_tab (dname,posn) gerror =    
    case M.lookup dname sym_tab of 
         Nothing -> error $
                      gerror ++ genLnNoError posn ++ 
                      "Symbol < " ++ dname ++ " > not in scope."
         Just sym_entry -> do   
               case sym_entry of 
                   S_CONSTR slist -> map (\(s,_,nargs) -> (s,nargs)) slist
                   S_DESTR  slist -> map (\(s,_,nargs) -> (s,nargs)) slist
                   _              -> error $
                                         gerror ++ genLnNoError posn ++ 
                                         "< " ++ dname ++ "> is not a data or a codata."        


look_up_syms_posn ::  SYM_TBL -> (NamePnPair,NamePnPair) -> SYM_VALUES
look_up_syms_posn sym_tab (s1pn@(s1,posn1),s2pn@(s2,posn2)) =
                      look_up_syms_helper entry s2pn s1pn  
           where
              val1  = M.lookup s1 sym_tab
              msg0  = genLnNoError posn1 
              msg1  = "Symbol <"++ s1 ++ "> not in scope."
              entry = maybeValToVal val1 (msg0 ++ msg1)

look_up_syms_helper :: SYM_ENTRY -> NamePnPair -> NamePnPair ->  SYM_VALUES
look_up_syms_helper entry (s2,pn2) (s1,pn1)  = case entry of
  S_CONSTR constrs -> SYM_CONSTR n m  
            where 
                conslist = map (\(x,y,z) -> (x,(y,z))) constrs
                val      = lookup s2 conslist
                msg      = genLnNoError pn2 ++  
                           "< " ++ s2 ++ " >" ++ "is not a constructor of "
                           ++ "< " ++ s1 ++ " >" ++ "data type."
                (n,m)    = maybeValToVal val msg 

  S_DESTR destrs  -> SYM_DESTR n m  
            where 
                destrlist = map (\(x,y,z) -> (x,(y,z))) destrs
                val       = lookup s2 destrlist
                msg       = genLnNoError pn2 ++  
                            "< " ++ s2 ++ " >" ++ "is not a destructor of "
                            ++ "< " ++ s1 ++ " >" ++ "codata type."
                (n,m)     = maybeValToVal val msg 

  S_HAND hands    -> SYM_HANDLE n
            where 
                val = lookup s2 hands
                msg = genLnNoError pn2 ++  
                      "< " ++ s2 ++ " >" ++ "is not a handle of " ++
                      "< " ++ s1 ++ " >" ++ "protocol type."                 
                n   = maybeValToVal val msg      

  S_CHAND chands    -> SYM_COHANDLE n
            where 
                val = lookup s2 chands 
                msg = genLnNoError pn2 ++  
                      "< " ++ s2 ++ " >" ++ "is not a cohandle of " ++
                      "< " ++ s1 ++ " >" ++ "coprotocol type." 
                n   = maybeValToVal val msg 

  otherwise  -> error $ genLnNoError pn1 ++  
                       "Symbol < "++s1++" > is a function or a process not a type."     

--------------------------------------------------------------------------------
look_up_syms_structs ::  SYM_TBL -> (NamePnPair,NamePnPair) ->  [String]
look_up_syms_structs sym_tab ((s_1,pos1),(s_2,pos2)) = case entry of 
                      S_CONSTR constrs  ->  map (\(s,_,_) -> s) constrs 
                      S_DESTR  destrs   ->  map (\(s,_,_) -> s) destrs 
                      S_HAND   hands    ->  map (\(s,_) -> s) hands
                      S_CHAND  hands    ->  map (\(s,_) -> s) hands
         where
               msg   = "Symbol <"++s_1++"."++s_2++"> not in scope."
               msg0  = genLnNoError pos1
               val   = M.lookup s_1 sym_tab
               entry = maybeValToVal val (msg0 ++ msg)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--Utilities for compiling
--------------------------------------------------------------------------------
type TRANSLATION = [(String,POLARITY,Int)]

-- translate takes a channel name (which is string) and a
-- translation(which is a translation of channel name, channel
-- number and polarity) and returns back a channel number


translate_posn :: TRANSLATION -> NamePnPair -> ErrorMsg -> Int 
translate_posn trans ch_pn gerror = ch_num
   where
      (ch_num,_) = lookup_trans_posn trans ch_pn gerror



translate_list_posn :: TRANSLATION -> [NamePnPair] -> ErrorMsg -> [Int]    
translate_list_posn trans nmpns gerror =
     map (\x -> translate_posn trans x gerror) nmpns


lookup_trans_posn :: TRANSLATION -> NamePnPair -> ErrorMsg -> (Int,POLARITY)
lookup_trans_posn trans (str,pair) gerror = 
                        maybeValToVal val (gerror ++ msg1) 
        where 
          msg1   = equals ++ "Channel name < "++ str ++ " " ++
                   genLnNoError1 pair ++ " > not in scope" ++ equals
          trans' = map (\(x,y,z) -> (x,(z,y))) trans 
          val    = lookup str trans' 

--------------------------------------------------------------------------------
-- restrict translation allows only those elements to be in the translation that 
-- are there in strs
--------------------------------------------------------------------------------
restrict_translation:: TRANSLATION -> [String] -> TRANSLATION
restrict_translation trans strs = filter (\(x,_,_) -> inlist' x strs) trans
--------------------------------------------------------------------------------
next_channel_num :: Int -> TRANSLATION -> Int 
next_channel_num m [] = m
next_channel_num m ((_,_,n):rest)
            | m <= n    = next_channel_num (n+1) rest
            | otherwise = next_channel_num m rest
--------------------------------------------------------------------------------






-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

store_args [] = []
store_args (_:rest) = AMC_STORE:(store_args rest)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
star   = "************************************************************************\n\n"
equals = "\n========================================================================\n"

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
genLnNoErrorspcl :: PosnPair -> ErrorMsg
genLnNoErrorspcl (line,col) = "(Line " ++ show line  
                           ++ ",Col " ++ show col ++ ")\n"


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
genLnNoError :: PosnPair -> ErrorMsg
genLnNoError (line,col) = "At (Line " ++ show line  
                           ++ ",Col " ++ show col ++ ")"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
genLnNoError1 :: PosnPair -> ErrorMsg
genLnNoError1 (line,col) = "(Line " ++ show line  
                            ++ ",Col " ++ show col ++ ")"
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
globalError :: (String,NamePnPair) -> ErrorMsg
globalError (fp,(fname,posn)) =
    case fp of
        "fun"       -> equals ++ "Error: Inside function < " ++ 
                       fname ++ " > defined at " ++ genLnNoErrorspcl posn  
        "proc"      -> equals ++ "Error: Inside process < " ++
                       fname ++ " > defined at " ++ genLnNoErrorspcl posn 
        "main_run"  -> equals ++ "Error:Inside main process "
                        ++ genLnNoErrorspcl posn   


depth_in_stack_posn :: [String] -> NamePnPair -> ErrorMsg -> Int 
depth_in_stack_posn stack (arg,posn) gerror = maybeValToVal val msg 
              where 
                strIntMap = M.fromList $ zip stack nums
                val       = M.lookup arg strIntMap
                msg0      = genLnNoError posn 
                msg       = gerror ++ equals ++ msg0 ++ 
                            " Variable <"++ arg ++"> not in scope!" ++ equals


--------------------------------------------------------------------------
-------------------------------------------------------------------------------
load_args_posn :: [String] -> [NamePnPair] -> ErrorMsg -> [AMPLCOM]
load_args_posn stack args emsg = map ( \arg -> AMC_LOAD
                                              (depth_in_stack_posn stack arg emsg) 
                                     )
                                     (
                                      reverse args 
                                     )  
