module CMPL.CompileProcess(compile_process,compile_common) where 
    
import AMPL.TypesAMPL
import CMPL.SymbolTable

import  Data.List as List 
import qualified  Data.Map as M 
import qualified Data.List.Split as DLS 
import qualified Data.Set as Set 
import Control.Monad.Trans.State.Lazy

commsStr_fun :: [String]
commsStr_fun = [
                 "AC_STOREf,AC_LOADf,AC_RET,AC_CALLf,AC_INT,AC_LEQ,AC_EQ,AC_MUL,AC_ADD,AC_SUB", 
                 "AC_DIVQ,AC_DIVR,AC_CHAR,AC_LEQC,AC_EQC,AC_STRING,AC_EQS,AC_LEQS,AC_CONCAT",
                 "AC_CONCATf,AC_STRUCT,AC_CASEf","AC_RECORDf","AC_TOSTR","AC_TOINT"
               ] 
           
commsList :: [String]
commsList = (concat.map (DLS.splitOn ",")) commsStr_fun

convCOMtoString :: COM -> String
convCOMtoString  = head.words.show 

compile_common:: [COM] -> State (SYM_TBL,STACK_STR,TRANSLATION,(String,NamePnPair)) [AMPLCOM]                 
compile_common []         = return []
compile_common pcoms@(cf:rest)  = do
    (sym,stack,trans,ginf) <- get 
    let gerror = globalError ginf 
    case cf of
         AC_STOREf st_posn (var,posn) -> do
            modify $ \(sy,st,tr,pr) -> (sy,(var:stack),tr,pr)
            rest' <- compile_common rest
            return $ AMC_STORE :rest'

         AC_LOADf ld_posn varpn       -> do           
            let 
               m = depth_in_stack_posn stack varpn gerror                
            rest' <- compile_common rest 
            return $ AMC_LOAD m :rest'
                
         AC_RET ret_posn              ->  do
            rest' <- compile_common rest 
            return $ AMC_RET:rest'

         AC_CALLf cl_posn fposn args  ->  do 
            helper_CALLf cl_posn fposn args rest 

         AC_INT int_posn (n,_)  -> do       -- INTEGER FUNCTIONS START
            rest' <- compile_common rest 
            return $ (AMC_INT n):rest'

         AC_LEQ leq_posn        -> do
            rest' <- compile_common rest 
            return $ AMC_LEQ:rest' 

         AC_EQ eq_posn          -> do
            rest' <- compile_common rest 
            return $ AMC_EQ:rest'

         AC_MUL mul_posn        -> do
            rest' <- compile_common rest 
            return $ AMC_MUL:rest'

         AC_ADD add_posn        -> do
            rest' <- compile_common rest 
            return $ AMC_ADD:rest'

         AC_SUB sub_posn        -> do
            rest' <- compile_common rest  
            return $ AMC_SUB:rest'

         AC_DIVQ divq_posn       -> do 
            rest' <- compile_common rest 
            return $ AMC_DIVQ:rest'
         AC_DIVR divr_posn       -> do
            rest' <- compile_common rest 
            return $ AMC_DIVR:rest'

         AC_CHAR chr_posn (c,posn)  -> do    -- CHAR FUNCTIONS START
            rest' <- compile_common rest 
            return $ (AMC_CHAR c):rest'

         AC_LEQC leqc_posn         -> do 
            rest' <- compile_common rest 
            return $ AMC_LEQC:rest'

         AC_EQC eqc_posn          -> do
            rest' <- compile_common rest 
            return $ AMC_EQC:rest'
            
         AC_STRING str_posn (str,posn)  -> do    -- STRING FUNCTIONS START
            rest' <- compile_common rest
            return $ (AMC_STRING str) : rest' 
       
         AC_LEQS leqs_posn        -> do 
            rest' <- compile_common rest
            return $ AMC_LEQS : rest'

         AC_EQS eqs_posn         -> do 
            rest' <- compile_common rest
            return $ AMC_EQS : rest'

         AC_CONCAT ct_posn n     -> do
            rest' <- compile_common rest
            return $ AMC_CONCATf n : rest'

         AC_UNSTRING un_posn     -> do 
           rest' <- compile_common rest
           return $ AMC_UNSTRING:rest'

         AC_TOSTR un_posn     -> do 
           rest' <- compile_common rest
           return $ AMC_TOSTR:rest'

         AC_TOINT un_posn     -> do 
           rest' <- compile_common rest
           return $ AMC_TOINT:rest'

         AC_STRUCT (s1posn,s2posn) vars_posns      -> do 
            helper_STRUCT (s1posn,s2posn) vars_posns rest  
         
         AC_CASEf cposn cases    -> do 
            cases' <- helper_cases cases
            modify $ \(sy,st,tr,pr) -> (sy,stack,tr,pr)
            rest'  <- compile_common rest 
            return $ (AMC_CASE cases' )  : rest'

         AC_RECORDf rposn recs   -> do 
            recs' <- helper_cases recs 
            modify $ \(sy,st,tr,pr) -> (sy,stack,tr,pr)
            rest' <- compile_common rest 
            return $ (AMC_REC recs'):rest'  

         AC_PROD vars_posns           -> do 
            let largs = load_args_posn stack vars_posns gerror
            rest' <- compile_common rest 
            return $ largs ++ (AMC_PROD (length vars_posns): rest')   

         AC_PRODELEM posn_prod n npn@(name,posnname) -> do
            let largs = load_args_posn stack [npn] gerror
            rest' <- compile_common rest  
            return $ largs ++ ((AMC_PRODELEM n ): rest')    

         AC_EMSG emsg -> 
            return $ [AMC_ERROR emsg] 

         othcomm  -> do 
            case fst ginf of
                "fun"      -> error $ "Process code in a function???!"
                                       ++ show (cf:rest) 
                "proc"     -> compile_process pcoms 
                "main_run" -> compile_process pcoms 

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

helper_CALLf :: PosnPair -> NamePnPair -> [NamePnPair] -> [COM] -> 
                State (SYM_TBL,STACK_STR,TRANSLATION,(String,NamePnPair)) [AMPLCOM]
helper_CALLf posn1 fpn@(fname,posn2) args_posns coms = do 
   (sym,stack,trans,ginf) <- get 
   let 
     gerror = globalError ginf 
     largs = load_args_posn stack args_posns gerror
     msg0  = genLnNoError posn1
     msg1  = "Function <"++ fname ++ "> is not defined." 
   case look_up_sym_posn sym fpn gerror of
         SYM_FUN n -> do 
             case n == length args_posns of
                 True -> do  
                    amplcoms <- compile_common coms 
                    return $ largs ++ 
                             ((AMC_CALL fname n):amplcoms)  
                 False -> 
                    error $ gerror ++ equals 
                             ++ msg0 ++  " Function < " ++ fname ++
                            " > called with incorrect number of arguments.(Expected:" ++
                            ((show.length) args_posns) ++ ",Actual:" ++ show n ++ ")." ++ equals 
         _    -> error $ gerror ++ equals ++ msg0 ++ msg1 ++ equals                  


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

helper_STRUCT :: (NamePnPair,NamePnPair) -> [NamePnPair] -> [COM] -> 
                 State (SYM_TBL,STACK_STR,TRANSLATION,(String,NamePnPair)) [AMPLCOM]   
helper_STRUCT ((s1,posn1),(s2,posn2)) vars_posns rest = do
    (sym,stack,trans,ginf) <- get 
    let
      gerror = globalError ginf 
      msg0  = genLnNoError posn2 
      msg1  = "non constructors/destructors name used as structor!"
    case look_up_syms_posn sym ((s1,posn1),(s2,posn2)) of
            SYM_CONSTR i n -> 
                  case n == length vars_posns of
                        True -> do 
                            let largs = load_args_posn stack vars_posns gerror 
                            rest' <- compile_common rest 
                            return $ largs ++(AMC_CONS i n ): rest'
                        False -> do     
                            error $  gerror ++ equals ++ msg0 ++ emsg ++ equals
                where 
                  emsg = ": Constructor <" ++ s1 ++ "." ++ s2 ++ 
                         " > applied to incorrect number of arguments.(Actual:" ++
                         (show.length) vars_posns ++ ",Expected:" ++ show n  ++ ")"  

            SYM_DESTR i n -> 
                  case  n + 1 == length vars_posns of 
                      True -> do 
                          let largs  = load_args_posn stack (tail vars_posns) gerror  
                              hlargs = load_args_posn stack [head vars_posns] gerror
                          rest' <- compile_common rest 
                          return $ hlargs ++ largs ++  (AMC_DEST i n ):rest'
                      False ->
                          error $ gerror ++ msg0 
                                 ++ "Destructor " ++ emsg
                where  
                  emsg =  "Destructor <" ++ s1 ++ "." ++ s2 ++ 
                          " > applied to incorrect number of arguments. (Expected :: " ++
                          (show.length) vars_posns ++ ", Actual :: " ++ show n                 
            _             -> error $ gerror ++ msg0 ++ msg1 
 

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
helper_cases  :: [(STRUCTOR_NAME,[NamePnPair],COMS)] -> 
                State (SYM_TBL,STACK_STR,TRANSLATION,(String,NamePnPair)) [AMPLCOMS] 
helper_cases cases = do 
      (sym,stack,trans,ginf) <- get 
      let 
        gerror      = globalError ginf 
        -- all structors in the arranged sequence are obtained.
        all_structs = (look_up_syms_structs sym.fst'.head) cases 
        cases'      = sort_cases all_structs cases gerror
      helper_cases_1 cases'

--------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------
sort_cases :: [String] -> [(STRUCTOR_NAME,[NamePnPair],COMS)] -> ErrorMsg -> [(STRUCTOR_NAME,[NamePnPair],COMS)]
sort_cases [] [] _               =  []
-------------------------------------------------------------------------------------
sort_cases rem_conses [] gerror  = 
      error $
        gerror ++ 
        "In Case command, pattern matching not done for following constructors.\n" ++
        (concat $ map (\x ->  x ++ "\n") rem_conses)

----------------------------------------------------------------------------------------  
sort_cases [] rem_list gerror    =  
      error $ 
        gerror ++
        "Extraneous constructors for data type " ++ (fst.fst.fst'.head) rem_list ++ ".\n"
        ++ msg1
   where
     rem_conses = map (snd.fst') rem_list
     msg1       = concat $ map gen_Error_message rem_conses

----------------------------------------------------------------------------------------                                 
sort_cases (c:crest) list gerror = 
            case length get_str  of 
              1 -> (head get_str) : sort_cases crest list' gerror
              -- this is error
              n -> error $ 
                        gerror ++ "Constructor <  " ++ (fst s2pn) ++ " > of data type < " ++ 
                        (fst s1pn) ++ " > used < " ++ show n ++ " > times for casing." ++ 
                        genLnNoErrorspcl (snd s2pn)
      where
         -- get the structor corresposnding to c  
         get_str     = get_req_cons c list
         -- the constructor that has been used multiple times for casing
         (s1pn,s2pn) = (fst'.head) get_str
         -- new list 
         list' = list List.\\ get_str

get_req_cons :: String -> [(STRUCTOR_NAME,[NamePnPair],COMS)] -> [(STRUCTOR_NAME,[NamePnPair],COMS)]
get_req_cons str list = filter (\(sname,args,coms) -> (fst.snd) sname == str ) list 
--------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------

helper_cases_1 :: [(STRUCTOR_NAME,[NamePnPair],COMS)] ->
                  State (SYM_TBL,STACK_STR,TRANSLATION,(String,NamePnPair)) [AMPLCOMS]
helper_cases_1 list_coms  = do 
  (sym,stack,trans,ginf) <- get 
  let gerror =  globalError ginf 
-- check all the constructors are of the same data type  
  case length dnames of 
          -- Can proceed
          0 -> do 
             -- get all the structors of the data type
             let conses = get_all_structors sym fd_name gerror
             amplcomms  <- check_structors_wellformed list_coms conses 
             return amplcomms  
          -- error as casing can't be done on multiple data types
          _ -> do 
             let
                msg0   = "Can't case on multiple data types.\n"
                msg1   = (concat.map gen_Error_message) (fd_name:dnames)
             error $ gerror ++ msg0 ++  msg1
     where
        list   = map (\(x,y,_) -> (x,y)) list_coms 
        dnames = (get_diff_dtypes.get_STR_Names_posn) list 
        -- get the first data name with posn ,its structors and its argument 
        fd_name      = (fst.fst.head) list
        -- get the different data names with posn used in casing                         
        get_STR_Names_posn :: [(STRUCTOR_NAME,[NamePnPair])] -> [NamePnPair]
        get_STR_Names_posn = map (\((d,_),_) -> d ) 
        -- get the list of different data types apart from first one that are 
        -- being used for casing
        get_diff_dtypes :: [NamePnPair] -> [NamePnPair]
        get_diff_dtypes = filter (\(name,posn) -> name /= fst fd_name )


gen_Error_message :: NamePnPair -> ErrorMsg
gen_Error_message (name,posn) = name ++ " : " ++ genLnNoErrorspcl posn 

-- check that each structor is well formed, each structor is present and then compile it.
-- second argument is the list of structor names along with how many arguments does each
-- constructor takes.  
--------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------
check_structors_wellformed :: [(STRUCTOR_NAME,[NamePnPair],COMS)] -> [(Name,Int)] -> 
                             State (SYM_TBL,STACK_STR,TRANSLATION,(String,NamePnPair)) [AMPLCOMS]

check_structors_wellformed [] [] = return []

check_structors_wellformed (((s1posn,s2posn),args_posns,coms):rest) tot_list = do 
      (sym,stack,trans,ginf) <- get
      let gerror     =  globalError ginf
          (s2,posn2) = s2posn
      -- check if the s2posn constructor is present in the second list.
      case lookup s2 tot_list of 
          Just nargs -> do 
            -- check if nargs is same as length of args_posns
              case nargs == length args_posns of
                 -- if condition holds then compile  
                 True -> do 
                    let args = map (\(x,posnx) -> x) args_posns
                        -- modify the second argument by removing the current
                        -- constructor
                        tot_list' = filter (\(n,i) -> n /= s2) tot_list 
                    modify $ \(sy,st,tr,gi) -> (sym,args++st,tr,gi)
                    amplcoms   <- compile_common coms 
                    modify $ \(sy,st,tr,gi) -> (sym,stack,trans,gi)
                    amplcomms  <- check_structors_wellformed rest tot_list'
                    case fst ginf of
                        "proc"  ->
                             return (amplcoms:amplcomms)
                        "main_run" ->
                            return (amplcoms:amplcomms)
                        "fun"  -> 
                              return (amplcoms:amplcomms)
                 -- if the number of arguments are unequal
                 False -> error $
                            gerror ++ equals ++ genLnNoError posn2 ++
                            ": Constructor < " ++ s2 ++ " > of < " ++ fst s1posn ++ " > has wrong number of arguments."
                            ++ "(Expected:" ++ show nargs ++ ",Actual:" ++ ((show.length) args_posns)
                            ++ ")."  ++ equals
          -- s2posn is an extraneous constructor    
          Nothing -> do 
               error $ 
                 gerror ++ ": Extraneous constructor for data type " ++ (fst s1posn) ++ ".\n"
                  ++ s2 ++ " : " ++ genLnNoErrorspcl posn2

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

fst' :: (a,b,c) -> a
fst' (x,y,z) = x 

snd' :: (a,b,c) -> b 
snd' (x,y,z) = y 

third' :: (a,b,c) -> c 
third' (x,y,z) = z 

---------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------


compile_process:: [COM] -> State (SYM_TBL,STACK_STR,TRANSLATION,(String,NamePnPair)) [AMPLCOM]
compile_process [] = return []
compile_process pcoms@(cf:rest) = do
    (sym,stack,trans,ginf) <- get 
    let gerror = globalError ginf 
    case (convCOMtoString cf `elem` commsList) of
        True -> do 
            amplcoms <- compile_common pcoms 
            return amplcoms
        False -> do 
            case cf of       
                AC_GETf gposn (var,posn1) chpn -> do 
                    let 
                       (cn,_) = lookup_trans_posn trans chpn gerror
                    rest'  <- compile_process rest 
                    return $ (AMC_GET cn):rest' 

                AC_HPUTf hposn chpn (s1posn,s2posn)       -> do
                    let 
                      (cn,pol) = lookup_trans_posn trans chpn gerror
                      s = look_up_syms_posn sym (s1posn,s2posn)
                      i = report_handle_num pol s (s1posn,s2posn,chpn) gerror
                    rest' <- compile_process rest   
                    return $ (AMC_HPUT cn i): rest' 

                AC_PUTf _ chpn                -> do
                   let (cn,_) = lookup_trans_posn trans chpn gerror 
                   rest' <- compile_process rest 
                   return $ AMC_PUT cn: rest'

                AC_IDf _ ch_pn1 ch_pn2            -> do
                    let t1 = translate_posn trans ch_pn1 gerror
                        t2 = translate_posn trans ch_pn2 gerror
                    rest' <- compile_process rest
                    return $ (AMC_ID t1 t2 ): rest' 

                AC_CLOSEf _ chpn               -> do
                    let (cn,_) = lookup_trans_posn trans chpn gerror
                    rest' <- compile_process rest 
                    return $ AMC_CLOSE cn : rest' 

                AC_HALTf _ ch_pns                -> do
                    let cns = map (\x -> fst (lookup_trans_posn trans x gerror)
                                  ) ch_pns 
                    rest' <- compile_process rest
                    --modify (\(rsym,st,t,g) -> (sym,stack,trans,g))
                    return $ AMC_HALT cns : rest'

                AC_HCASEf _ ch_pn labcoms       -> do
                   let (cn,pol) = lookup_trans_posn trans ch_pn gerror
                   comlist <- helper_hcase pol labcoms  
                   modify $ \(sy,st,tr,gi) -> (sym,stack,trans,ginf) -- don't need this. Think carefully and check
                   rest'   <- compile_process rest
                   --modify (\(rsym,st,t,g) -> (sym,stack,trans,g))
                   return $ (AMC_HCASE cn comlist):rest'

                AC_PLUGf _ ch_pn_list (chns_posns_1,cs1) (chns_posns_2,cs2) -> do
                    modify (\(sym,st,trans,ginf) -> (sym,stack,trans1,ginf)) 
                    rest1' <- compile_process cs1
                    modify $ \(sym,st,trans,ginf) -> (sym,stack,trans2,ginf)
                    rest2' <- compile_process cs2
                    --modify (\(rsym,st,t,g) -> (sym,stack,trans,g))
                    return $ (AMC_PLUG ch_list (chs1,rest1') (chs2,rest2')):[] 
                 where
                      len'     = length ch_pn_list
                      ch_list  = next_channel_list trans len'
                      chs1 = translate_list_posn trans chns_posns_1 gerror
                      chs2 = translate_list_posn trans chns_posns_2 gerror
                      t1   = restrict_translation trans ( map fst chns_posns_1 )
                      t2   = restrict_translation trans ( map fst chns_posns_2 )
                      chn_list = map fst ch_pn_list
                      trans1   = (zipWith (\chn ch -> (chn,OUT,ch)) chn_list ch_list) ++ t1 
                      trans2   = (zipWith (\chn ch -> (chn,IN, ch)) chn_list ch_list) ++ t2 
         
                AC_RUNf pnpr (pname,posn) vars (in_chs,out_chs)       -> do
                    let larg = 
                            load_args_posn stack vars gerror
                        newtrans = 
                            helper_RUNf_proc (pnpr,gerror) in_chs out_chs 
                                             (pname,posn) vars sym trans 
                    modify (\(rsym,st,t,g) -> (sym,stack,trans,g))
                    return $ larg ++[(AMC_RUN newtrans pname (length vars))]

                AC_SPLITf pnpr ch_pn ((ch1,pn1),(ch2,pn2))           -> do
                    modify (\(sym,stack,trans,ginf) -> (sym,stack,trans',ginf)) 
                    rest' <- compile_process rest
                    modify (\(rsym,st,t,g) -> (rsym,stack,trans,g))
                    return $ AMC_SPLIT ch' (ch1',ch2'): rest' 
                 where 
                   (ch',pol) = lookup_trans_posn trans ch_pn gerror         
                   ch1' = next_ch 1 trans
                   ch2' = ch1'+1
                   trans' = (ch1,pol,ch1'):((ch2,pol,ch2'):trans)
                   next_ch n [] = n
                   next_ch n ((_,p,m):rest)
                           |m >= n = next_ch (m+1) rest
                           | otherwise = next_ch n rest

                AC_FORKf pnpr ch_pn ((ch_pn1,chns_pn1,cs1),(ch_pn2,chns_pn2,cs2)) -> do
                    modify (\(sy,st,tr,gin)-> (sym,stack,trans1,ginf))
                    cs1'  <- compile_process cs1
                    modify (\(sy,st,tr,gin)-> (sym,stack,trans2,ginf))
                    cs2'  <- compile_process cs2
                    modify (\(sy,st,tr,ginf)-> (sym,stack,trans,ginf))
                    rest' <- compile_process rest  
                    --modify (\(rsym,st,t,g) -> (sym,stack,trans,g))                    
                    return $ AMC_FORK ch ((ch1,chs1,cs1'),(ch2,chs2,cs2')) : rest' 
                 where 
                   (ch,pol)    = lookup_trans_posn trans ch_pn gerror
                   (chn1,pn1)  = ch_pn1
                   (chn2,pn2)  = ch_pn2 
                   chns1       = map fst chns_pn1 
                   chns2       = map fst chns_pn2
                   m           = next_channel_num 1 trans
                   trans1 = (chn1,pol,m):
                            (restrict_translation trans chns1)
                   ch1    = translate_posn trans1 ch_pn1 gerror
                   chs1   = translate_list_posn trans1 chns_pn1 gerror
                   m'     = next_channel_num m trans1
                   trans2 = (chn2,pol,m'):
                            (restrict_translation trans chns2)
                   ch2    = translate_posn trans2 ch_pn2 gerror
                   chs2   = translate_list_posn trans2 chns_pn2 gerror

                str -> error $ "Error in following.\n" ++ show str ++ "\n" ++ show pcoms


isBuiltIn :: CHANNEL -> Bool 
isBuiltIn (terminal,_) 
  = elem (init terminal) ("intTerm","charTerm") 


next_channel_list :: TRANSLATION -> Int -> [Int]
next_channel_list  trans len = take len [(last+1)..]
                   where 
                    trans_chans = (sort.map (\(x,y,z) -> z)) trans
                    len_trans   = length trans_chans
                    last        = trans_chans !! (len_trans-1)

--------------------------------------------------------------------------------
-- Helper Function for AC_HPUTf
--------------------------------------------------------------------------------
report_handle_num :: POLARITY -> SYM_VALUES -> 
                    (NamePnPair,NamePnPair,NamePnPair) -> ErrorMsg -> Int 
report_handle_num OUT (SYM_HANDLE i) _   gerror                 = i
report_handle_num IN  (SYM_COHANDLE i) _ gerror                 = i
report_handle_num _ _ ((s1,posn1),(s2,posn2),(ch,posn3)) gerror =
                       error $                           
                           gerror ++ equals ++ genLnNoError1 posn1 ++ ":" ++
                           "Handle < " ++ s1 ++"."++s2 ++ " > on channel < "
                           ++ ch ++ " > of wrong polarity!!" ++ equals

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
mkUniq :: Ord a => [a] -> [a]
mkUniq = (Set.toList) .(Set.fromList)

helper_hcase :: POLARITY -> [(STRUCTOR_NAME,[COM])] -> 
                State (SYM_TBL,STACK_STR,TRANSLATION,(String,NamePnPair)) [AMPLCOMS]
helper_hcase pol labcoms = do 
    (sym,stack,trans,ginf) <- get 
    let
       gerror     = globalError ginf  
       -- get all protocol names
       prot_names = map (fst.fst) labcoms
       only_prots = (mkUniq.map fst) prot_names
       comss      = map snd labcoms
       struct     = (head.map fst) labcoms
       orih_hs    = look_up_syms_structs sym struct 
       errors     = (concat.map gen_Error_message) prot_names
       sorted     = sort_handles orih_hs labcoms gerror  
    -- check that hcasing is being done on only one protocol
    case length only_prots of
      1 -> do 
        -- check if it is a handle and cohandle and take into account the polarity
        case (look_up_syms_posn sym struct,pol) of
            -- allowed 
            (SYM_HANDLE _ , IN)  -> do 
              amplcomms <- helper_hcase_genCode comss 
              return amplcomms
            (SYM_COHANDLE _,OUT) -> do 
              amplcomms <- helper_hcase_genCode comss 
              return amplcomms
            -- erorr
            (SYM_HANDLE _ ,OUT)  -> do 
              error $
                 gerror ++ equals ++
                 genLnNoError1 ((snd.fst) struct) ++ ":" ++
                 "Can't Hcase a handler on an output channel." ++ equals 

            (SYM_COHANDLE _,IN)  -> do   
              error $
                 gerror ++ equals ++
                 genLnNoError1 ((snd.fst) struct) ++ ":" ++
                 "Can't Hcase a cohandler on an input channel." ++ equals         
      n -> do 
        error $
          gerror ++ equals ++
          "Trying to Hcase on " ++ show n ++ " different protocols/coprotocols.\n" ++ 
          errors ++ equals


helper_hcase_genCode :: [COMS] -> State (SYM_TBL,STACK_STR,TRANSLATION,(String,NamePnPair)) [AMPLCOMS]
helper_hcase_genCode []     = return []
helper_hcase_genCode (c:cs) = do 
      (sym,stack,trans,ginf) <- get 
      c'  <- compile_process c 
      modify $ \(sy,st,tr,gi) -> (sym,stack,trans,ginf)
      cs' <- helper_hcase_genCode cs 
      return (c':cs') 

-------------------------------------------------------------------------------
--------------------------------------------------------------------------------

sort_handles :: [String] -> [(STRUCTOR_NAME,[COM])] -> ErrorMsg -> [(STRUCTOR_NAME,[COM])]

sort_handles [] [] _             = []
--------------------------------------------------------------------------------
sort_handles rem_hs [] gerror    = 
    error $
       gerror ++ equals ++ 
       "Following handles/cohandles haven't been hcased on." ++
       (concat.map (\x -> x ++ "\n")) rem_hs

--------------------------------------------------------------------------------
sort_handles [] rem_pairs gerror =
    error $
       gerror ++ equals ++
       "Extraneous handles in the hcase command." ++
        errors ++ equals
  where
    rem    = map (\((p,h),cs) -> h) rem_pairs  
    errors = (concat.map gen_Error_message) rem      
--------------------------------------------------------------------------------
sort_handles (h:hs) pairs gerror = 
    case length fh of
        1 -> (head fh) : sort_handles hs pairs' gerror
        -- hcasing has been done multiple times on a handle (error)  
        n -> error $
               gerror ++ equals ++
               "Following handles have been hcased on " ++ show n ++ " times."
               ++ errors ++ equals                       
       where 
        -- get the structor corresposnding to h  
        fh      = filter (\((pr,ha),co) -> fst ha == h ) pairs 
        fh_snds = map (snd.fst) fh 
        errors  = (concat.map gen_Error_message) fh_snds
        pairs'  = pairs List.\\ fh  
--------------------------------------------------------------------------------
--- Helper Function for AC_CALLf
-------------------------------------------------------------------------------- 
helper_RUNf_proc :: (PosnPair,ErrorMsg) ->
                    [NamePnPair] -> [NamePnPair] -> NamePnPair ->
                    [NamePnPair] -> SYM_TBL -> TRANSLATION -> TRANS 
helper_RUNf_proc (posn1,gerror) in_chs_posns out_chs_posns 
                 (pname,posn2) vars_posns sym trans    =  
    case cond of 
        (True,True,True)  -> 
           zipWith (\y (p,x) -> (y,p,x)) [1..] list  
        (False,True,True) -> 
          error $ 
               gerror ++ equals ++ genLnNoError posn1 ++ 
               "\nProcess < "++ pname ++ error_fun "Channel (Input)"
        (True,False,True) -> 
          error $
               gerror ++ equals ++ genLnNoError posn1 ++ 
               "\nProcess < "++ pname ++ error_fun "Channel (Output)" 
        (True,True,False) -> 
          error $
               gerror ++ equals ++ genLnNoError posn1 ++ 
               "\nProcess < "++ pname ++ error_fun "Sequential " 
    where
       cond = (
               length in_chs_posns  == ins , 
               length out_chs_posns == outs , 
               length vars_posns    == arity 
              ) 

       list = (map (\x -> (IN,x)) in_chs')++
              (map (\x -> (OUT,x)) out_chs')

       error_fun :: String -> String 
       error_fun str = 
          " > run with inappropriate " ++ str ++ " arguments!"
           ++ equals 

       (arity,(ins,outs))  = 
            case look_up_sym_posn sym (pname,posn2) gerror of
                 SYM_PROCESS arity (ins,outs) -> (arity,(ins,outs))
                 _ -> error $
                         gerror ++  equals ++ 
                         "Expected < "++ pname ++ " " ++
                          genLnNoError1 posn2 ++ " > to be a process" ++ equals
       in_chs' = 
           map ( \(x,posnx)  ->
                      case lookup_trans_posn trans (x,posnx) gerror of
                          (y,IN) -> y
                          _ -> error $ 
                                 gerror ++ equals ++
                                 "Channel < "++ x ++ " " ++ 
                                 genLnNoError1 posnx ++ " > has wrong polarity!" ++ equals
               ) in_chs_posns 

       out_chs' =
           map ( \(x,posnx) -> 
                     case lookup_trans_posn trans (x,posnx) gerror of
                          (y,OUT) -> y
                          _ -> error $
                                 gerror ++ 
                                 equals ++ "Channel < "++ x ++ " " ++
                                 genLnNoError1 posnx ++ " > has wrong polarity!" ++ equals
                ) out_chs_posns