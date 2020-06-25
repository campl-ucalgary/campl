module CMPL.CMPLtoPTree where 

import qualified CMPL.TypesCoreMPL as C 
import qualified AMPL.TypesAMPL as A 
import AMPL.AbsAMPL

import Control.Monad.State.Lazy 
import qualified Data.Map as M 
import Data.List 


getFresh_Var_Name :: State Int String
getFresh_Var_Name  = do 
    newInt <- get 
    modify (\n-> n+1)
    return $ "v_r"  ++ show newInt
-------------------------------------------------------------------------
-------------------------------------------------------------------------
convProtocol :: C.Protocol_Defns -> AMPL_CONSTRUCTS
convProtocol prots = HANDLE_CONSTRUCT
                        (
                          Handles (treeifyProtocols map_prots)
                        )
                 where
                    map_prots = map removeProts prots 
                    removeProts :: C.Defn -> (A.PosnPair,A.NamePnPair,[A.NamePnPair])
                    removeProts (C.Protocol (pair,(nmPair,nmPairs))) = (pair,nmPair,nmPairs)


treeifyProtocols :: [(A.PosnPair,A.NamePnPair,[A.NamePnPair])] -> [HANDLE_SPEC]
treeifyProtocols ps = map convHANDLE_SPEC ps 
                    
convHANDLE_SPEC :: (A.PosnPair,A.NamePnPair,[A.NamePnPair]) -> HANDLE_SPEC
convHANDLE_SPEC (posn1,(nm,posn2),hs) = Hand_spec (convUIdent (nm,posn1))
                                                  (map convHandle hs) 

-------------------------------------------------------------------------
-------------------------------------------------------------------------

convCoProtocol :: C.CoProtocol_Defns -> AMPL_CONSTRUCTS
convCoProtocol coprots = COHANDLE_CONSTRUCT
                        (
                          Cohandles (treeifyCoProtocols map_coprots)
                        )
                 where
                    map_coprots = map removeCoProts coprots 
                    removeCoProts :: C.Defn -> (A.PosnPair,A.NamePnPair,[A.NamePnPair])
                    removeCoProts (C.CoProtocol (pair,(nmPair,nmPairs))) = (pair,nmPair,nmPairs)


treeifyCoProtocols :: [(A.PosnPair,A.NamePnPair,[A.NamePnPair])] -> [HANDLE_SPEC]
treeifyCoProtocols ps = map convHANDLE_SPEC ps 
                 
-------------------------------------------------------------------------
-------------------------------------------------------------------------

convData :: C.Data_Defns -> AMPL_CONSTRUCTS
convData datas = treeifyData map_conses
        where  
            map_conses = map (\x -> removeData x) datas 
            removeData :: C.Defn -> (A.PosnPair,A.NamePnPair,[C.Constructor])
            removeData (C.Data (posn1,(nmpair,conses))) = (posn1,nmpair,conses) 

treeifyData :: [(A.PosnPair,A.NamePnPair,[C.Constructor])] -> AMPL_CONSTRUCTS  
treeifyData lists = CONSTRUCTOR_CONSTRUCT
                              ( Constructors
                                   (
                                     map convSTRUCTOR_SPEC lists
                                   )
                              )    
         
convSTRUCTOR_SPEC ::  (A.PosnPair,A.NamePnPair,[C.Constructor]) -> STRUCTOR_SPEC
convSTRUCTOR_SPEC (posn1,(nm,posn2),cs) = Struct_spec
                                                  (UIdent (posn1,nm))  
                                                  (map convSTRUCT cs)


convSTRUCT :: C.Constructor -> STRUCT
convSTRUCT (name,n) = Struct (UIdent $ swap name) (pintegerify n)     

pintegerify :: (Int,A.PosnPair) -> PInteger
pintegerify (int,pair) = PInteger (pair,show int)

-------------------------------------------------------------------------
-------------------------------------------------------------------------


convCodata :: C.Codata_Defns -> AMPL_CONSTRUCTS
convCodata datas = treeifyCodata map_dests
        where  
            map_dests = map (\x -> removeCodata x) datas 
            removeCodata ::  C.Defn -> (A.PosnPair,A.NamePnPair,[C.Constructor])
            removeCodata (C.Codata (posn1,(nmpair,conses))) = (posn1,nmpair,conses) 

treeifyCodata :: [(A.PosnPair,A.NamePnPair,[C.Constructor])] -> AMPL_CONSTRUCTS  
treeifyCodata lists = DESTRUCTOR_CONSTRUCT
                              ( Destructors
                                   (map convSTRUCTOR_SPEC lists)
                              )    
         


-------------------------------------------------------------------------
-------------------------------------------------------------------------

convFunctions :: C.Function_Defns -> State Int AMPL_CONSTRUCTS
convFunctions funs = do
      fspecs <- convFSPECS funs
      return $
        FUNCTIONS_CONSTRUCT
                       (
                        Functions fspecs  
                       )



convFSPECS :: [C.Defn] ->  State Int [FUNCTION_SPEC]
convFSPECS [] = return []
convFSPECS (d:ds) = do
      d'  <- convFunction d 
      ds' <- convFSPECS ds 
      return (d':ds')  

convFunction :: C.Defn -> State Int FUNCTION_SPEC
convFunction (C.Function (posn1,funcName,argpn_list,term)) = do 
            cterm <- convTerm term
            return $ Function_spec 
                          ( convPIdent (fst fnPn,posn1))
                          ( map strPnToVar argpn_list)
                          ( Prog $ cterm ++
                              [
                               AC_RET (Ret (posn1,"ret")) 
                              ]
                          )
               where 
                 fnPn = get_Func_Name_Posn funcName

                     
-------------------------------------------------------------------------
-------------------------------------------------------------------------

convTerms :: [C.Term] -> State Int [COM]
convTerms []     = return [] 
convTerms (t:ts) = do 
    t'  <- convTerm  t 
    ts' <- convTerms ts 
    return (t'++ ts')


convTerm :: C.Term -> State Int [COM]  
convTerm t  = 
    case t of
        C.TCall (fname,terms) -> 
           case fname of
               C.Custom nmposn -> 
                   case notVars of
                       [] -> do 
                          let arg_pn_list = map (\(C.TVar (v,pn)) -> (v,pn)) terms
                          return  [
                                    AC_CALLf  (Call (snd nmposn,"call")) 
                                              (convPIdent nmposn)
                                              (map convPIdent arg_pn_list)
                                  ]         
                       _  -> do 
                          (fstrs_posn,coms) <- convTCall_Custom terms ([],[]) 
                          return $ coms ++ [
                                             AC_CALLf (Call (snd nmposn,"call")) 
                                                      (convPIdent nmposn)
                                                      (map convPIdent fstrs_posn)
                                           ]            
                  where 
                    notVars = filter (\x -> not $ isTVar x) terms
                      
               C.Inbuilt (ifun,posn) -> do
                   cterms <- convTerms terms
                   return $ cterms ++ [fcom]                   
                  where
                     fcom   = get_command ifun posn 
        ---------------------------------------------------
        C.TCons ((dnameposn,consposn),terms) -> do 
             (fstrs_posn,coms) <- convTCall_Custom terms ([],[]) 
             return $ coms ++ [ AC_STRUCTas (convUIdent dnameposn)
                                            (convUIdent consposn)
                                            (map convPIdent fstrs_posn)
                              ]            
        ---------------------------------------------------
        C.TDest (pidposn,(cnameposn,destposn),terms) -> do 
              (fstrs_posn,coms) <- convTCall_Custom terms ([],[]) 
              return $ [ 
                         AC_LOADf (Load (snd pidposn,"load"))
                                  (convPIdent pidposn)  
                       ] ++
                       coms ++
                       [
                         AC_STRUCTas (convUIdent cnameposn) 
                                     (convUIdent destposn ) 
                                     (map convPIdent fstrs_posn)    
                       ]

        ---------------------------------------------------
        C.TCase (term,pattdefs,posn) -> do 
               cterm    <- convTerm term 
               labcomss <- convpattdefs pattdefs                              
               return $ cterm ++ [AC_CASEf (Case (posn,"case")) labcomss]
        ---------------------------------------------------
        C.TVar  str_posn       -> return [
                                            AC_LOADf (Load (snd str_posn,"load"))
                                                     (convPIdent str_posn)
                                         ]
       ----------------------------------------------------
        C.TConstS str_posn     -> return [
                                           AC_STRING (ConstString (snd str_posn,"cString"))
                                                     (fst str_posn)
                                                     --(convCString str_posn)
                                         ] 
       ----------------------------------------------------
        C.TConstI int_posn     -> return [
                                           AC_INT (ConstInt (snd int_posn,"cInt"))
                                                  (intPosnToCInteger int_posn)
                                         ]
        ----------------------------------------------------
        C.TConstC (char,posn)  -> return [
                                           AC_CHAR (ConstChar (posn,"cChar"))
                                                   (Character (posn,show char))
                                         ]
        ------------------------------------------------------
        C.TRec (plist,posn) -> do
                       labcomss <- convTRecs plist 
                       return $ [AC_RECORDf (Rec (posn,"rec")) labcomss]
        ------------------------------------------------------
        C.TProd terms       -> do 
                      (fstrs_posn,coms) <- convTCall_Custom terms ([],[]) 
                      return $ coms ++ [
                                         AC_PROD (map convPIdent fstrs_posn)
                                       ]   

        C.TProdElem (n,term,posn) -> do 
                      (str_posn:[],coms) <- convTCall_Custom [term] ([],[]) 
                      return $ coms ++ [
                                         AC_PRODELEM (intPosnToCInteger (n,snd str_posn))
                                                     (convPIdent str_posn)
                                       ]   
        
        C.TError emsg -> 
                      return $ [AC_EMSG emsg]  
                             


-------------------------------------------------------------------------
-------------------------------------------------------------------------
getTermPosn :: C.Term -> A.PosnPair
getTermPosn term = 
    case term of
        C.TCall (fname,_)   -> 
            case fname of
                C.Custom  (_,posn) -> posn 
                C.Inbuilt (_,posn) -> posn 
        C.TCons (st_name,_) ->
                (snd.fst) st_name
        C.TCase (term,_,_)  -> 
                getTermPosn term
        C.TVar (_,posn)     -> posn
        C.TConstS (_,posn)  -> posn  
        C.TConstC (_,posn)  -> posn
        C.TConstI (_,posn)  -> posn
        C.TRec    (_,posn)  -> posn
        C.TProd   ts        -> getTermPosn (head ts)
        C.TProdElem (_,_,pn)-> pn 
        C.TError _          -> (0,0)    




-------------------------------------------------------------------------
-------------------------------------------------------------------------

convpattdefs :: [C.PatternDef] ->  State Int [LABELCOMS]
convpattdefs []     = return []
convpattdefs (p:ps) = do 
     p'  <- convpattdef p 
     ps' <- convpattdefs ps 
     return (p':ps')

-------------------------------------------------------------------------
-------------------------------------------------------------------------

convpattdef :: C.PatternDef ->  State Int LABELCOMS
convpattdef ((dname_posn,cname_posn,args_posn),terms) = do 
      cterms <- convTerms terms
      let com = Labelcoms2 ( convUIdent dname_posn)
                           ( convUIdent cname_posn)
                           ( map convPIdent args_posn)
                           ( 
                             Prog (
                                   cterms ++ [
                                               AC_RET (Ret (snd dname_posn,"ret") )
                                             ]
                                  )
                           )
      return com                      


-------------------------------------------------------------------------
-------------------------------------------------------------------------

-- This will be a mixture of vars and non vars
-- separate them
-- nothing to do for vars
-- for nonvars evaluate the term and store them
-- and keep the list of all the variable names that
-- are generated.
convTCall_Custom :: [C.Term] -> ([A.NamePnPair],[COM]) -> 
                    State Int ([A.NamePnPair],[COM])
convTCall_Custom [] (stlist,comms) = do 
                   return ( (reverse stlist),
                            comms
                          )
-- in the end reverse the string and 
convTCall_Custom (t:ts) (strs_posn,coms) = 
    case t of
        C.TVar s_posn -> do 
           convTCall_Custom ts ((s_posn:strs_posn),coms)
        othTerm -> do 
           cTerm <- convTerm t 
           fvar  <- getFresh_Var_Name 
           let tposn = getTermPosn t
               fvar_posn = (fvar,tposn) -- to chnage the position
               ncoms =  coms ++ cTerm 
                        ++  [
                             AC_STOREf (Store (snd fvar_posn, "store")) 
                                       (convPIdent fvar_posn)
                            ]
           convTCall_Custom ts (fvar_posn:strs_posn,ncoms)


get_Func_Name_Posn :: C.FuncName -> (String,A.PosnPair) 
get_Func_Name_Posn fname = 
    case fname of 
        C.Custom (str,pair)  -> (str,pair) 



--------------------------------------------------------------------
--------------------------------------------------------------------
convMain :: C.MainRun_Defn -> State Int START
convMain (C.MainRun (posn,ichs_posn,ochs_posn,pcoms)) = do 
   pcoms' <- convProcessCommands pcoms 
   let chspec = Channel_specf (map convPIdent ichs_posn)
                              (map convPIdent ochs_posn)
   return $ Start (Main_run (posn,"%run")) 
                  chspec 
                  (Prog pcoms')                           

--------------------------------------------------------------------
--------------------------------------------------------------------

convProcesses :: C.Process_Defns -> State Int AMPL_CONSTRUCTS
convProcesses pdefns = do 
   pspecs <- convProcesses_helper pdefns
   return $ PROCESSES_CONSTRUCT 
                              (
                                Processes pspecs
                              ) 
                    
convProcesses_helper :: [C.Defn] -> State Int [PROCESS_SPEC]
convProcesses_helper  [] = return [] 
convProcesses_helper  (p:ps) = do
    p' <- convProcess p 
    ps' <- convProcesses_helper ps 
    return (p':ps')



convProcess :: C.Defn -> State Int  PROCESS_SPEC 
convProcess (C.Process (posn1,namepn, args_posn, ichs_posn, ochs_posn, pcoms)) = do 
    pcoms' <- convProcessCommands pcoms  
    return $
       Process_spec (convPIdent (fst namepn,posn1))
                    (map strPnToVar args_posn)
                    (map convPIdent ichs_posn)
                    (map convPIdent ochs_posn)
                    (Prog pcoms')



--------------------------------------------------------------------
--------------------------------------------------------------------

convProcessCommand :: C.ProcessCommand -> State Int [COM]
convProcessCommand com = do
    case com of
         C.PRun (posn,name_posn,terms,ichs_posn,ochs_posn) -> do 
                (fstrs_posn,comms) <- convTCall_Custom terms ([],[]) 
                return $ comms ++ [AC_RUNf ( Run (posn,"run"))
                                           ( convPIdent name_posn)
                                           ( map convPIdent fstrs_posn)
                                           ( map convPIdent ichs_posn)
                                           ( map convPIdent ochs_posn) 
                                 ]        
         -------------------------------------------------------- 
         C.PClose (posn, ch_posn) -> do 
               return $ [
                         AC_CLOSEf (Close (posn,"close"))
                                   (convPIdent ch_posn) 
                        ]
         --------------------------------------------------------
         C.PHalt (posn,chs_posn) -> do 
               return $ [ 
                         AC_HALTf (Halt (posn,"halt"))
                                  (map convPIdent chs_posn) 
                        ]
         --------------------------------------------------------
         C.PGet (posn,str_posn,chs_posn) -> do 
               return $ [ 
                          AC_GETf (Get (posn,"get"))
                                  (convPIdent str_posn)
                                  (convPIdent chs_posn),
                          AC_STOREf (Store (snd str_posn,"store"))
                                    (convPIdent str_posn)         
                                           
                        ]            
         --------------------------------------------------------
         C.PPut (posn,term,channel_posn) -> do
                coms <- convTerm term
                return $ coms ++ [ 
                                 AC_PUTf (Put (posn,"put"))
                                         (convPIdent channel_posn)
                                ] 
         --------------------------------------------------------
         C.PHcase (posn,ch_posn,pphr_hcases) -> do 
              hcases' <- convHCases pphr_hcases
              return $ [
                        AC_HCASEf (Hcase (posn,"hcase"))
                                  (convPIdent ch_posn)
                                  hcases'
                       ]
         --------------------------------------------------------
         C.PHput (posn,(nameposn,handposn),ch_posn) -> do 
              return $ [ 
                         AC_HPUTf (Hput (posn,"hput"))
                                  (convPIdent ch_posn)
                                  (convUIdent nameposn)
                                  (convUIdent handposn) 
                       ]          
         --------------------------------------------------------
         C.PSplit (posn,ch_posn,ch1_posn:ch2_posn:[]) -> do 
              return $ [ 
                         AC_SPLITf (Split (posn,"split"))
                                   (convPIdent ch_posn)
                                   (convPIdent ch1_posn)
                                   (convPIdent ch2_posn) 
                       ]            
         --------------------------------------------------------
         C.PPlug (posn,chs1_pn,(chs2_pn,proc1),(chs3_pn,proc2)) -> do 
              coms1 <- convProcessCommands proc1
              coms2 <- convProcessCommands proc2
              return $ [ 
                         AC_PLUGf (Plug (posn,"plug"))
                                  (map convPIdent chs1_pn)
                                  (map convPIdent chs2_pn)
                                  (Prog coms1) 
                                  (map convPIdent chs3_pn)
                                  (Prog coms2)
                       ]           
         --------------------------------------------------------
         C.PFork (posn,ch_posn,f1:f2:[]) -> do 
              (i1,is1,coms1) <- convForkPart f1 
              (i2,is2,coms2) <- convForkPart f2
              return $ [ AC_FORKf (Fork (posn,"fork"))
                                  (convPIdent ch_posn)
                                  i1 is1 coms1
                                  i2 is2 coms2
                       ]
         --------------------------------------------------------              
         C.PEqual (posn,ch1_pn,ch2_pn) ->    do 
             return $ [ AC_IDF (convPIdent ch1_pn)
                               (Ch_Id (posn,"==") )
                               (convPIdent ch2_pn)  

                      ]
         --------------------------------------------------------
         C.PCase (posn,term,pcases) -> do       
                       cterm    <- convTerm term 
                       labcomss <- convPCases pcases                              
                       return $ cterm ++ [
                                           AC_CASEf (Case (posn,"case")) labcomss
                                         ]
         --------------------------------------------------------
         C.PRec (posn,plist)       -> do 
                       labcomss <- convTRecs_Proc plist 
                       return $ [AC_RECORDf (Rec (posn,"rec")) labcomss]                       



-------------------------------------------------------------------------
-------------------------------------------------------------------------
convTRecs :: [(C.Struct_Handle,C.Term)] -> State Int [LABELCOMS]
convTRecs []     = return []
convTRecs (r:rs) = do 
       rec'  <- convTRec r
       recs' <- convTRecs rs 
       return (rec':recs') 

convTRec :: (C.Struct_Handle,C.Term) -> State Int LABELCOMS
convTRec ((cdata_pn,dest_pn,args_pn),term) = do 
       term' <- convTerm term
       return $ Labelcoms2 (convUIdent cdata_pn   )
                           (convUIdent dest_pn    )
                           (map convPIdent args_pn)
                           (Prog ( term' ++ 
                                  [AC_RET (Ret((0,0),"Ret"))]
                                 )
                           )

-------------------------------------------------------------------------
-------------------------------------------------------------------------
convTRecs_Proc :: [(C.Struct_Handle,[C.ProcessCommand])] -> State Int [LABELCOMS]
convTRecs_Proc []     = return []
convTRecs_Proc (r:rs) = do 
       rec'  <- convTRec_Proc r
       recs' <- convTRecs_Proc rs 
       return (rec':recs') 

convTRec_Proc :: (C.Struct_Handle,[C.ProcessCommand]) -> State Int LABELCOMS
convTRec_Proc ((cdata_pn,dest_pn,args_pn),pcoms) = do 
       pcoms' <- convProcessCommands pcoms
       return $ Labelcoms2 (convUIdent cdata_pn   )
                           (convUIdent dest_pn    )
                           (map convPIdent args_pn)
                           (Prog pcoms')

                    

-------------------------------------------------------------------------
-------------------------------------------------------------------------
convPCases :: [C.ProcessPhrase_pcase] ->  State Int [LABELCOMS]
convPCases []     = return []
convPCases (p:ps) = do 
      p'   <- convPCase p 
      ps'  <- convPCases ps
      return (p':ps')

convPCase :: C.ProcessPhrase_pcase -> State Int LABELCOMS 
convPCase ((namepn,conspn,args_pn),proc) = do
        proc' <- convProcessCommands proc 
        return $ Labelcoms2 (convUIdent namepn)
                            (convUIdent conspn)
                            (map convPIdent args_pn)
                            (Prog proc') 


-------------------------------------------------------------------------
-------------------------------------------------------------------------
convForkPart :: C.ForkPart -> State Int (PIdent,[PIdent],COMS)
convForkPart (ch_posn,chs_posn,proc) = do 
      proc' <- convProcessCommands proc 
      return (
               (convPIdent ch_posn),
               (map convPIdent chs_posn),
               Prog proc'
             )

-------------------------------------------------------------------------
-------------------------------------------------------------------------
convHCases ::[C.ProcessPhrase_hcase] -> State Int [LABELCOMS]
convHCases []     = return []
convHCases (p:ps) = do 
     p'  <- convHCase p 
     ps' <- convHCases ps 
     return (p':ps')              

-------------------------------------------------------------------------
-------------------------------------------------------------------------
convHCase :: C.ProcessPhrase_hcase -> State Int LABELCOMS
convHCase ((namepn,handpn),pcoms) = do
          pcoms' <- convProcessCommands pcoms 
          return $ Labelcoms1 (convUIdent namepn)
                              (convUIdent handpn) 
                              (Prog pcoms')


-------------------------------------------------------------------------
-------------------------------------------------------------------------
convProcessCommands :: [C.ProcessCommand] -> State Int [COM]
convProcessCommands []         = return []  
convProcessCommands (com:coms) = do 
         com'  <- convProcessCommand com 
         coms' <- convProcessCommands coms 
         return (com' ++ coms')
                              
-------------------------------------------------------------------------
-------------------------------------------------------------------------

convHandle :: A.NamePnPair -> Handle 
convHandle nmposn = HandName (convUIdent nmposn)


convUIdent :: A.NamePnPair -> UIdent
convUIdent nmposn = UIdent (swap nmposn) 

convPIdent :: A.NamePnPair -> PIdent
convPIdent nmposn = PIdent (swap nmposn)

strPnToVar :: A.NamePnPair -> Vars 
strPnToVar nmposn = VName (convPIdent nmposn)

isTVar :: C.Term -> Bool
isTVar (C.TVar _) = True 
isTVar _          = False 

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

convPInteger :: (Int,A.PosnPair) -> PInteger 
convPInteger (int,posn) = PInteger (posn, show int)

intPosnToCInteger:: (Int,A.PosnPair) -> CInteger 
intPosnToCInteger nposn@(n,posn) 
       | n >= 0    = Positive (convPInteger nposn)
       | otherwise = Negative (convPInteger nposn)

get_command :: C.Func -> A.PosnPair ->  COM
get_command c posn = case c of
   C.Add_I -> AC_ADD   (Add(posn,"add")) 
   C.Sub_I -> AC_SUB   (Subtract(posn,"subtract")) 
   C.Mul_I -> AC_MUL   (Mul (posn,"mul")) 
   C.DivQ_I  -> AC_DIVQ(Quot(posn,"quot"))
   C.DivR_I  -> AC_DIVR(Rem (posn,"rem")) 
   C.Eq_I    -> AC_EQ  (EqI (posn,"eq"))  
   C.Leq_I   -> AC_LEQ (LeqI(posn,"leq")) 
   C.Eq_C    -> AC_EQC (Eqc (posn,"eqc")) 
   C.Leq_C   -> AC_LEQC (Leqc (posn,"leqc")) 
   C.Eq_S    -> AC_EQS  (Eqs  (posn,"eqs")) 
   C.Leq_S   -> AC_LEQS (Leqs (posn,"leqs"))
   C.Concat_S n    -> AC_CONCAT (ConcatS  (posn,"concatS")) (toInteger n )
   C.Unstring_S    -> AC_UNSTRING (Unstring (posn,"unstring"))
   C.Append        -> AC_APPEND (Append (posn,"appendL"))
   C.ToStr         -> AC_TOSTR (ToStr (posn,"toStr"))
   C.ToInt         -> AC_TOINT (ToInt (posn,"toInt"))
   C.Or_B          -> AC_OR  (Or  (posn,"or"))
   C.And_B         -> AC_AND (And (posn,"and") )





-------------------------------------------------------------------------
-------------------------------------------------------------------------

convEverything_helper :: C.MPLProg -> State Int AMPLCODE
convEverything_helper 
      (C.MPLProg _ dds cds pcolds cpcolds funcds procds mrund) = do 
          let
            dds'     = convData  dds 
            cds'     = convCodata cds 
            pcolds'  = convProtocol pcolds
            cpcolds' = convCoProtocol cpcolds

          funcds'  <- convFunctions funcds
          procds'  <- convProcesses procds
          mrund'   <- convMain mrund
          return $ Main 
                     (dds' : cds' : pcolds' : cpcolds' : funcds' : procds':[])
                     mrund' 



convEverything :: C.MPLProg -> AMPLCODE
convEverything mplprog = evalState (convEverything_helper mplprog)
                                   0 


