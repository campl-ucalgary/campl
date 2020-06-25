module CMPL.CompileAll (compile_all) where

import AMPL.TypesAMPL
import CMPL.CompileProcess
import CMPL.SymbolTable 

import Data.List
import Control.Monad.Trans.State.Lazy


import qualified  Data.Map as M 


------------------------------------------------------------------------------------------------------
-- Top level compilation
------------------------------------------------------------------------------------------------------

compile_process_defn :: PROCESS_SPEC -> State SYM_TBL DEFN  
compile_process_defn (Process_specf  (pname,pposn) vars_posns (in_chs_posns,out_chs_posns) cs) = do
    sym <- get 
    let vars = map fst vars_posns
        amcs = evalState (compile_process cs)
                         (sym,vars,t,("proc",(pname,pposn)))
        l1            = map (\(x,_) -> (x,IN))  in_chs_posns
        l2            = map (\(x,_) -> (x,OUT)) out_chs_posns
        t             = zipWith (\(x,p) n -> (x,p,n)) (l1 ++ l2) nums  
    return $ (pname,amcs)



compile_function_defn :: FUNCTION_SPEC ->  State SYM_TBL  DEFN
compile_function_defn  fsp@(Function_specf (fname,fposn) vars_posns cs) = do
   sym <- get
   --tell $ ["Inside compile_function_defn function \n" ++ star ]
   let vars = map fst vars_posns
       amcs = evalState (compile_common cs) (sym,vars,[],("fun",(fname,fposn)))
   return $ (fname,amcs)

compile_function_defn  fsp@(Function_spec (fname,fposn) cs) = do
   sym <- get
   --tell $ ["Inside compile_function_defn  \n"  ++ star ]
   let amcs = evalState (compile_common cs) (sym,[],[],("fun",(fname,fposn)))
   return $ (fname,amcs) 


compile_run :: (PosnPair,CHANNEL_SPEC,COMS) -> State SYM_TBL (TRANS,[AMPLCOM])
compile_run (posn,Channel_specf in_chs_posn out_chs_posn,coms) = do 
     sym <- get 
     let code = evalState (compile_process coms) (sym,[],trans,("main_run",("main_run",posn)))
         l1 = map (\(x,posnx) -> (x,IN,services_AMPL IN x posnx)) in_chs_posn
         l2 = map (\(x,posnx) -> (x,OUT,services_AMPL OUT x posnx)) out_chs_posn
         trans = l1 ++ l2 
         trans' = map (\(_,p,x) -> (x,p,x)) trans
     return $ (trans',code) 


------------------------------------------------------------------------------------------------------
--
-- Services provided by AMPL
--
------------------------------------------------------------------------------------------------------

services_AMPL IN term posn 
         | term == "console" = 0
         | term == "cconsole" = -100
services_AMPL OUT term posn 
         | term' == "int" = assign_int_chan term posn 
         | term' == "cha" = assign_char_chan term posn   
         where 
           term' = take 3 term 

services_AMPL _ str posn = error $ equals ++ genLnNoError posn ++ 
                                   "\nService < "++str++" > not supported."
                                   ++ equals 


assign_int_chan :: String -> (Int,Int) -> Int
assign_int_chan str posn 
          | -99 <= chn_no && chn_no <= -1 = chn_no
          | otherwise = error $ genLnNoError posn ++ 
                                "Error:Using wrong Integer Terminals"
          where 
            diff   = str \\ "intTerm"
            chn_no = negate (read diff ::Int)
                                               
                      
assign_char_chan :: String -> (Int,Int) -> Int
assign_char_chan str posn 
          | -199 <= chn_no && chn_no <= -101 = chn_no
          | otherwise = error $ genLnNoError posn ++ 
                               "Error:Using wrong Character Terminals"
          where 
            diff   = str \\ "charTerm"
            chan   = "10" ++ diff
            chn_no = negate (read chan ::Int)
                                               



compile_all :: AMPLCODE -> MACH  
compile_all prog@(AMPLcode _ _ _ _ ps fs cs) = ([mach_st]::PROCESSES,chm::CHM,defns::DEFNS)
   where  sym = collect_symbols prog
          pdefns   = map (process_defn_helper sym) ps
          fdefns   = map (function_defn_helper sym) fs
          defns    = pdefns ++ fdefns              
          (trans,code) = evalState (compile_run cs) sym 
          chs = map (\(x,_,_) -> (x,x)) trans
          empty_queues = map (\(x,y) -> (x,Q_EMPTY,Q_EMPTY)) chs
          mach_st = ([],trans,[],code)
          chm = (chs,empty_queues)
  


          
process_defn_helper ::SYM_TBL -> PROCESS_SPEC -> DEFN
process_defn_helper sym ps = evalState (compile_process_defn ps) (sym::SYM_TBL)

 


function_defn_helper ::SYM_TBL -> FUNCTION_SPEC -> DEFN
function_defn_helper sym fs =  evalState (compile_function_defn fs) sym



