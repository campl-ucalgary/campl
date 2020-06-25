module ToCMPL.MPLToCMPL where 
{-
import qualified CMPL.TypesCoreMPL as C
import qualified TypeInfer.MPL_AST as M 
import qualified TypeInfer.Gen_Eqns_CommFuns as Comm 
import qualified AMPL.TypesAMPL as A 
import Control.Monad.State
import Data.List 

type PairList = [(C.Name,C.Name)]

getFTypePn :: M.FunType -> A.PosnPair 
getFTypePn fType 
    = case fType of 
        M.NoType ->
          (0,0)
        M.StrFType (_,sType) -> 
          Comm.getTypePosn sType 
        M.IntFType (_,iType) ->
          Comm.getTypePosn iType  

convMPL :: M.MPL -> C.MPLProg
convMPL mpl = evalState (convMPL_help mpl) ([],[])  

convMPL_help :: M.MPL -> State (PairList,PairList) C.MPLProg
convMPL_help stmts = do 
    tDefn <- mapM convStmt stmts 
    let 
      finDefns= concat tDefn 
      idefns  = C.Includes []
      ddefns  = filter isCoreData   finDefns 
      cddefns = filter isCoreCodata finDefns 
      pdefns  = filter isCoreProt   finDefns 
      cpdefns = filter isCoreCoprot finDefns
      fundefns= filter isCoreFun    finDefns  
      procdefn= filter isCoreProc   finDefns 
      mainDefn= filter isCoreMain   finDefns
    return $ C.MPLProg idefns ddefns cddefns pdefns 
                       cpdefns fundefns procdefn (head mainDefn)  


isCoreData :: C.Defn -> Bool 
isCoreData defn 
    = case defn of 
        C.Data _  -> True 
        otherwise -> False 

isCoreCodata :: C.Defn -> Bool 
isCoreCodata defn 
    = case defn of 
        C.Codata _  -> True 
        otherwise   -> False 

isCoreProt :: C.Defn -> Bool 
isCoreProt defn 
    = case defn of 
        C.Protocol _ -> True 
        otherwise    -> False 

isCoreCoprot :: C.Defn -> Bool 
isCoreCoprot defn 
    = case defn of 
        C.CoProtocol _ -> True 
        otherwise      -> False

isCoreFun :: C.Defn -> Bool 
isCoreFun defn 
    = case defn of 
        C.Function _ -> True 
        otherwise    -> False 

isCoreProc :: C.Defn -> Bool 
isCoreProc defn 
    = case defn of 
        C.Process _ -> True 
        otherwise   -> False 

isCoreMain :: C.Defn -> Bool 
isCoreMain defn 
    = case defn of 
         C.MainRun _ -> True 
         otherwise   -> False  


getDatProt_Defns :: [M.Defn] -> (PairList,PairList) -> (PairList,PairList) 
getDatProt_Defns [] accList
    = accList
getDatProt_Defns (d:ds) accList
    = getDatProt_Defns ds newaccList
  where     
    newaccList = getDatProt_Defn d accList


getDatProt_Defn :: M.Defn -> (PairList,PairList) -> (PairList,PairList)
getDatProt_Defn defn (dcdList,pcpList) 
    = case defn of 
        M.Data _ ->
          (convData defn ++ dcdList,pcpList)
        M.Codata _ ->
          (convData defn ++ dcdList,pcpList)
        M.ProtocolDefn _ ->
          (dcdList,convProt defn ++ pcpList) 
        M.CoprotocolDefn _ -> 
          (dcdList,convProt defn ++ pcpList) 


convProt :: M.Defn -> [(C.Name,C.Name)]
convProt defn 
    = case defn of 
        M.ProtocolDefn (pcls,pn) -> 
          handlePcls pcls 

        M.CoprotocolDefn (pcls,pn) -> 
          handlePcls pcls  

handlePcls :: [M.ProtocolClause] -> [(C.Name,C.Name)]
handlePcls = (concat.map handlePcls_help.map transPcl)

handlePcls_help :: C.ProtocolClause -> [(C.Name,C.Name)]
handlePcls_help ((pnm,_),hnms)
    = map (\(h,hpn) -> (h,pnm)) hnms  

transPcl :: M.ProtocolClause -> C.ProtocolClause
transPcl (pName,pPhrs) 
      = ((pnm,hPosn),hands)
    where
      hands = map (\(n,fType) -> (n,getFTypePn fType)) pPhrs  
      hPosn = (snd.head) hands 
      M.DataName (pnm,_) 
            = pName  

convData :: M.Defn -> [(C.Name,C.Name)]
convData (M.Data  (dcls,pn))
        = handleDecls dcls 

convData (M.Codata(dcls,pn))
        = handleCdDecls dcls 

handleDecls :: [M.DataClause] -> [(C.Name,C.Name)]
handleDecls dcls
    = concat $ map (\((n,p),cs) -> map (\c -> ((fst.fst)c, n)) cs) tDcls 
  where 
      tDcls = map transDcl dcls 


handleCdDecls :: [M.DataClause] -> [(C.Name,C.Name)]
handleCdDecls dcls
    = concat $ map (\((n,p),cs) -> map (\c -> ((fst.fst)c, n)) cs) tDcls 
  where 
      tDcls = map transCdcl dcls 


transDcl :: M.DataClause -> C.DataClause
transDcl (dnm,dphrs)
        = ((nm,dpn),conses)
    where 
       M.DataName (nm,_) 
           = dnm
       conses
           = map transDPhr dphrs 
       dpn = (snd.fst.head) conses  

transCdcl :: M.DataClause -> C.DataClause
transCdcl (dnm,dphrs)
        = ((nm,dpn),conses)
    where 
       M.DataName (nm,_) 
           = dnm
       conses
           = map transCDPhr dphrs 
       dpn = (snd.fst.head) conses  

transDPhr :: M.DataPhrase -> C.Constructor 
transDPhr (nm,fType,num) 
        = ((nm,fPn),(num,fPn))
    where 
      fPn = getFTypePn fType 

transCDPhr :: M.DataPhrase -> C.Constructor 
transCDPhr (nm,fType,num) 
        = ((nm,fPn),(num-1,fPn))
    where 
      fPn = getFTypePn fType 

-- ===============================================
-- ===============================================

convStmt :: M.Stmt -> State (PairList,PairList) [C.Defn] 
convStmt stmt 
    = case stmt of 
        M.DefnStmt (defns,stmts,pn) -> do 
          (dcdList,pcpList) <- get
          case stmts == [] of 
            True  -> do 
              let
                pdDefns= filter Comm.isProtData defns
                (addDCD_D,addPCP_D) 
                   = getDatProt_Defns pdDefns ([],[])

              modify $ \(d,p) -> (d ++ addDCD_D,p ++ addPCP_D)         
              tDefns <- mapM convDefn defns 
              return tDefns                  

            False -> do 
              let 
                emsg = "There shouldn't be any statement in the" ++
                       "where part of defn" ++ Comm.printPosn pn  
              error emsg 


        M.RunStmt (fType,ichs,ochs,pcoms,pn) -> do 
          (ichsN,ochsN,pcomsN) <- genChPComTrip (ichs,ochs,pcoms,pn)   
          return [C.MainRun (pn,ichsN,ochsN,pcomsN)]


-- ===============================================
-- ===============================================

convDefn :: M.Defn -> State (PairList,PairList) C.Defn 
convDefn defn
    = case defn of 
        M.Data(dcls,pn) -> do 
            let 
              tDcls = map transDcl dcls
            return $ C.Data (pn,head tDcls)

        M.Codata(dcls,pn) -> do 
            let 
              tDcls = map transCdcl dcls
            return $ C.Codata (pn,head tDcls)

        M.TypeSyn (tsyms,pn) -> 
            undefined 

        M.ProtocolDefn (pcls,pn) -> do 
            let 
              tPcls = map transPcl pcls
            return $ C.Protocol (pn,head tPcls)

        M.CoprotocolDefn (pcls,pn) -> do 
            let 
              tPcls = map transPcl pcls
            return $ C.CoProtocol (pn,head tPcls)

        M.FunctionDefn (fnm,fType,pattTerms,pn) -> do 
            let 
              ((patts,eithTerm),_) 
                   = head pattTerms
              args = concat $ map pattToArgs patts 
              Left term 
                   = eithTerm 

            tTerm <- convTerm term                     
            return $ C.Function (pn,transName pn fnm,args,tTerm)

        M.ProcessDefn  (nm,_,pattProc,pn) -> do 
            let 
              args  = concat $ map pattToArgs patts 
              (patts,inchs,ochs,proc) 
                    = pattProc
              ichsN = map (\p -> (p,pn)) inchs 
              ochsN = map (\p -> (p,pn)) ochs
            (tIs,tOs,tProc) <- genChPComTrip (inchs,ochs,proc,pn)   
            return $ C.Process (pn,(nm,pn),args,tIs,tOs,tProc)   

        M.TermSyn termSyn ->
          undefined 

        M.OperatorDefn (str,eith,pn) ->    
          undefined 



-- ===============================================
-- ===============================================

transName :: M.PosnPair -> M.FuncName -> C.FuncName
transName pn fname 
    = case fname of 
        M.Custom nm   -> 
            C.Custom (nm,pn)

        M.BuiltIn fn  -> 
            C.Inbuilt (transBuiltIn fn,pn)


transBuiltIn :: M.Func -> C.Func         
transBuiltIn fn 
    = case fn of 
              M.Add_I -> 
                C.Add_I

              M.Sub_I -> 
                C.Sub_I

              M.Mul_I ->
                C.Mul_I

              M.DivQ_I ->
                C.DivQ_I

              M.DivR_I ->
                C.DivR_I

              M.Eq_I   ->
                C.Eq_I

              M.Neq_I  ->
                undefined

              M.Leq_I  ->
                C.Leq_I

              M.Geq_I  -> 
                undefined 

              M.LT_I   ->
                undefined  

              M.GT_I   ->
                undefined 

              M.Eq_C   ->
                C.Eq_C

              M.Eq_S   ->
                C.Eq_S

              M.Concat_S -> 
                C.Concat_S 2 

              M.Unstring_S ->
                C.Unstring_S

              M.ToStr ->
                C.ToStr

              M.ToInt ->
                C.ToInt 

              M.Append ->
                C.Append 

              M.Or_B  ->
                C.Or_B 

              M.And_B ->   
                C.And_B           


pattToArgs :: M.Pattern -> [C.Argument] 
pattToArgs (M.VarPattern vpn) 
    = [vpn] 
pattToArgs (M.ProdPattern (patts,pn)) 
    = concat (map pattToArgs patts) 
-- ===================================================================
-- ===================================================================

convPCom :: M.ProcessCommand -> 
            State (PairList,PairList) C.ProcessCommand
convPCom pcom 
    = case pcom of 
       M.PRun(nm,terms,ichs,ochs,pn) -> do 
          (ichsN,ochsN,pcomsN) <- genChPComTrip (ichs,ochs,[],pn)
          cTerms <- mapM convTerm terms  
          return $ C.PRun (pn,(nm,pn),cTerms,ichsN,ochsN)
                  
       M.PClose (ch,pn) -> 
          return $ C.PClose (pn,(ch,pn))

       M.PHalt  (ch,pn) ->
          return $ C.PHalt (pn,[(ch,pn)])

       M.PGet   (patt,ch,pn) ->
          return $ C.PGet (pn,head (pattToArgs patt),(ch,pn)) 

       M.PPut   (term,ch,pn) -> do 
          tTerm <- convTerm term 
          return $ C.PPut (pn,tTerm,(ch,pn))

       M.PHPut  (nm,ch,pn) -> do 
            (dcdlist,pcplist) <- get
            case lookup nm pcplist of 
              Just prot -> do 
                let 
                  handle = ((prot,pn),(nm,pn))
                return $ C.PHput (pn,handle,(ch,pn))

              Nothing -> do 
                let 
                  emsg 
                    = "No Protocol/Coprotocol found for handle/cohandle <<" 
                       ++ nm ++ ">>" ++ Comm.printPosn pn 
                error emsg 

       M.PHCase (ch,trips,pn) -> do 
           cTrips <- mapM handleProcTrip trips 
           return $ C.PHcase (pn,(ch,pn),cTrips)  

       M.PSplit (ch,(ch1,ch2),pn) -> do 
          return $ 
             C.PSplit (pn,(ch,pn),map (\c -> (c,pn)) [ch1,ch2])

       M.PFork  (str,trips,pn) -> do 
          cTrips <- mapM (handleForkTrip pn) trips 
          return $ C.PFork (pn,(str,pn),cTrips)      

       M.PPlug  (chs,((ch1,p1),(ch2,p2)),pn) -> do 
          let 
            chsN = map (\c -> (c,pn)) chs
            ch1N = map (\x -> (x,pn)) ch1
            ch2N = map (\x -> (x,pn)) ch2

          p1N <- mapM convPCom p1 
          p2N <- mapM convPCom p2 
          return $ C.PPlug (pn,chsN,(ch1N,p1N),(ch2N,p2N))

       M.PId(pch,ch,pn) ->  
          return $ C.PEqual (pn,(pch,pn),extractChan ch pn)    

       M.PCase (term,pattProcs,pn) -> do 
           pattPsN <- mapM handlePattProc pattProcs 
           termN   <- convTerm term
           return $ C.PCase (pn,termN,pattPsN) 

       M.PNeg(pch1,pch2,pn) -> 
          undefined


extractChan :: M.Channel -> M.PosnPair -> C.Channel 
extractChan chan pn 
    = case chan of
        M.PosChan ch  -> (ch,pn) 
        M.NegChan nch -> (nch,pn)  


-- ===================================================================
-- ===================================================================

handlePattProc :: (M.Pattern,M.Process) -> 
                  State (PairList,PairList) (C.Struct_Handle ,C.Process)
handlePattProc (patt,procs) = do 
    shand  <- convPatt patt 
    cProcs <- mapM convPCom procs
    return (shand,cProcs)

{-
Important Realiation - it is important to note here that all the patterns 
that are arguments of the constructors are var patterns
-}
convPatt :: M.Pattern ->
            State (PairList,PairList) C.Struct_Handle
convPatt (M.ConsPattern (cname,cpatts,pn)) = do  
    (dcdlist,pcplist) <- get 
    case lookup cname dcdlist of 
      Just datName -> do
          let 
            args = map (\(M.VarPattern vpn) -> vpn) cpatts   
          return ((datName,pn),(cname,pn),args) 

      Nothing -> do 
          let 
            emsg 
              = "No data type found for constructor <<" 
                 ++ cname ++ ">>" ++ Comm.printPosn pn 
          error emsg


handleProcTrip ::(M.Name,M.Process,M.PosnPair) ->
                 State (PairList,PairList) C.ProcessPhrase_hcase
handleProcTrip (hname,pcoms,pn) = do
    (dcdlist,pcplist) <- get 
    case lookup hname pcplist of 
        Just prot -> do 
          let 
            compProt = ((prot,pn),(hname,pn))
          newProcs <- mapM convPCom pcoms 
          return (compProt,newProcs)

        Nothing -> do 
          let 
            emsg 
              = "No Protocol/Coprotocol found for handle/cohandle <<" 
                 ++ hname ++ ">>" ++ Comm.printPosn pn 
          error emsg 
                  

genChPComTrip:: ([M.PChannel],[M.PChannel],M.Process,M.PosnPair) -> 
        State (PairList,PairList) ([C.Channel],[C.Channel],C.Process)
genChPComTrip (ichs,ochs,procs,pn) = do  
    let 
      ichsN = map (\n -> (n,pn)) ichs 
      ochsN = map (\n -> (n,pn)) ochs 
    tProcs <- mapM convPCom procs 
    return (ichsN,ochsN,tProcs)

handleForkTrip :: M.PosnPair -> (M.PChannel,[M.PChannel],M.Process) ->
      State (PairList,PairList) (C.Channel,[C.Channel],C.Process)
handleForkTrip pn (ch1,chs2,proc) = do 
    (ichs,ochs,procN) <- genChPComTrip ([ch1],chs2,proc,pn)
    return (head ichs,ochs,procN)

-- ===================================================================
-- ===================================================================
convTerm :: M.Term -> State (PairList,PairList) C.Term 
convTerm term 
    = case term of 
        M.TRecord trips -> do
          let
            tPosn = ((\(x,y,z)-> z).head) trips
          tRecs <- mapM handleRec trips
          return $ C.TRec(tRecs,tPosn)
           
        M.TCallFun (fname,terms,pn) -> do 
          tTerms <- mapM convTerm terms 
          return $ C.TCall(transName pn fname,tTerms) 

        M.TVar pair  -> 
          return $ C.TVar pair

        M.TConst(bval,pn)-> 
          case bval of
            M.ConstInt num -> 
              return $ C.TConstI (num,pn)
            M.ConstChar char ->
              return $ C.TConstC (char,pn)
            M.ConstString str ->
              return $ C.TConstS (str,pn)
            M.ConstDouble dbl ->
              undefined                 

        M.TCase (term,pattTermList,pn) -> do
          tTerm    <- convTerm term 
          pattDefs <- mapM convPattTerm pattTermList 
          return $ C.TCase (tTerm,pattDefs,pn)

        M.TCons (nm,terms,pn) -> do 
          (dcdlist,pcplist) <- get 
          case lookup nm dcdlist of 
            Just datName -> do 
              tTerms <- mapM convTerm terms  
              let 
                str_nm = ((datName,pn),(nm,pn))
              return $ C.TCons (str_nm,tTerms)

            Nothing -> do 
              let 
                emsg 
                  = "No data type found for constructor <<" 
                     ++ nm ++ ">>" ++ Comm.printPosn pn 
              error emsg 

        M.TDest (nm,terms,pn) -> do 
          (dcdlist,pcplist) <- get 
          case lookup nm dcdlist of 
            Just datName -> do 
              tTerms <- mapM convTerm terms 
              let 
                str_nm = ((datName,pn),(nm,pn))
              return $ C.TCons (str_nm,(last tTerms):(init tTerms))

            Nothing -> do 
              let 
                emsg 
                  = "No data type found for constructor <<" 
                     ++ nm ++ ">>" ++ Comm.printPosn pn 
              error emsg 

        M.TProd (terms,pn) -> do 
          tTerms <- mapM convTerm terms 
          return $ C.TProd tTerms

        M.TError emsg -> 
          return $ C.TError emsg 



-- ===================================================================
-- ===================================================================

convPattTerm :: M.PatternTermPhr -> State (PairList,PairList) C.PatternDef
convPattTerm (patt:[],Left term) = do
    (dcdlist,pcplist) <- get
    let 
      M.ConsPattern (cname,patts,pn) 
          = patt  
    case lookup cname dcdlist of
        Just datName -> do 
          tTerm <- convTerm term
          let
            args = concat $ map pattToArgs patts  
            str_hand = ((datName,pn),(cname,pn),args)
          return (str_hand,[tTerm])

        Nothing -> do   
          let 
            emsg = "No data type found for constructor <<" 
                   ++ cname ++ ">>" ++ Comm.printPosn pn 
          error emsg 




handleRec ::(M.Pattern,M.Term,M.PosnPair) ->
            State (PairList,PairList) (C.Struct_Handle,C.Term) 
handleRec (patt,term,_) = do
    (dcdlist,pcplist) <- get  
    let 
      M.DestPattern (dest,dpatts,dpn) 
             = patt 
      args   = concat $ map pattToArgs dpatts

    tTerm <- convTerm term  
    case lookup dest dcdlist of 
      Just dname -> do 
        let 
          st_hand = ((dname,dpn),(dest,dpn),args)
        return (st_hand,tTerm)

      Nothing -> do 
        let 
          emsg 
            = "No codata type found for destructor <<" 
               ++ dest ++ ">>" ++ Comm.printPosn dpn 
        error emsg         

-- ===================================================================
-- =================================================================== 

{-
getChans :: M.ProcessCommand -> [M.PChannel]
getChans pcom 
    = case pcom of 
        M.PRun(_,_,ichs,ochs,pn) ->
          (ichs ++ ochs)
        M.PClose (ch,pn) ->
          [ch] 
        M.PHalt (ch,pn) ->
          [ch]
        M.PGet (_,ch,pn) ->
          [ch]
        M.PPut (_,ch,pn) ->
          [ch] 
        M.PHPut (_,ch,pn) ->
          [ch] 
        M.PHCase (ch,_,pn) -> 
          [ch]
        M.PSplit(ch1,(ch2,ch3),pn) -> 
          [ch1,ch2,ch3]

        M.PFork(_,list,pn) -> 
          nub $ concat $ map (getChan_fork pn) list 

        M.PPlug  (pchs,(proc1,proc2),pn) ->
          nub $ pchs ++ (concat $ map getChans (proc1 ++ proc2))

        M.PId (pch,ch,pn) ->
          [pch,fst ch1] 
         where 
           ch1 = extractChan ch pn 
            
        M.PCase  (_,pattProcs,pn) -> 
          concat $ map getChans ((concat.map snd) pattProcs)

        M.PNeg   (ch1,ch2,pn) ->
          [ch1,ch2]  

getChan_fork :: M.PosnPair -> (M.PChannel,[M.PChannel],M.Process) -> [M.PChannel] 
getChan_fork pn (ch,chs,proc)
    = (ch:chs) ++ (concat $ map getChans proc) 
-}
-}
