module ToCMPL.LambdaLift where 

import TypeInfer.MPL_AST
import ToCMPL.GenAndSolveSetEqns
import ToCMPL.LamLift_Help

import qualified Data.Set as S 
import Data.List
import Control.Monad.State 

-- ============================================================
-- ============================================================
{-
This function will be used to handle only the defintions in the outer most scope
-}
lam_lift_sel :: Defn -> [Defn] 
lam_lift_sel fd 
      = case fd of 
            FunctionDefn (fnm,fT,pattTermPn,pn) -> 
                case lam_lift_req fd of
                    True  ->
                          fDefns ++ [newFD]
                      where 
                       ((patts,Left term),ppn)
                          = head pattTermPn
                       (newTerm,fDefns)
                          = lift_Term term 
                       newFD 
                          = FunctionDefn(fnm,fT,[((patts,Left newTerm),ppn)],pn)
                    
                    False ->
                      [fd]

            otherwise ->
              [fd]


{-
This is for lambda lifting the functions in the where clause. Before a function
in the where clause is lambda lifted it must be free of "let"clauses
in its body.
-}

get_rid_let :: Defn -> [Defn]
get_rid_let fd@(FunctionDefn (fnm,fT,pattTermPn,pn)) 
     = fDefns ++ [newFD]
   where 
     ((patts,Left term),ppn)
             = head pattTermPn
     (newTerm,fDefns)
             = lift_Term term 
     newFD   = FunctionDefn(fnm,fT,[((patts,Left newTerm),ppn)],pn)


lam_lift_req :: Defn -> Bool  
lam_lift_req fDefn@(FunctionDefn (_,_,pattTermPn,_)) 
      = or liftBool 
  where 
     terms    = map (removeLeft.snd.fst) pattTermPn     
     liftBool = map lift_req_term terms 


 -- ================================================================

lift_TermList :: [Term] -> ([Term],[Defn])
lift_TermList []
      = ([],[])

lift_TermList (t:ts)
      = (newT:tList,newD ++ dList)
  where
    (newT,newD)   = lift_Term t 
    (tList,dList) = lift_TermList ts 

-- ===================================================================

lift_Term :: Term -> (Term,[Defn]) 
lift_Term term 
    = case term of 
          TRecord recList ->
              (TRecord newRec,fDefns)   
            where
              (newRec,fDefns) = lift_Rec_help recList

          TCallFun (fname,terms,pn) ->
              (TCallFun (fname,nTerms,pn),fDefns)
            where
              (nTerms,fDefns) = lift_TermList terms    

          TLet(term,lwhrs,pn) ->
              (finT,d1 ++ whrDefns)
            where 
              (newT,d1) = lift_Term term
              (finT,whrDefns)
                        = handle_LetWhr lwhrs newT    
               
          
          TIf (t1,t2,t3,pn) ->
              (TIf (nT1,nT2,nT3,pn),ds1 ++ ds2 ++ ds3)
            where
              (nT1,ds1) = lift_Term t1
              (nT2,ds2) = lift_Term t2  
              (nT3,ds3) = lift_Term t3 


          TCase (term,pattTerms,pn) -> 
              (TCase (newT,newPattT,pn),d1 ++ newDefns)
            where
              (newT,d1) = lift_Term term 
              (newPattT,newDefns)  
                        = handlePattTerms pattTerms

          TFold (term,foldPatts,pn) -> 
              (TFold (newT,newFPatt,pn), d1 ++ newDefns)
            where
              (newT,d1) = lift_Term term 
              (newFPatt,newDefns)  
                        = handleFoldPatts foldPatts              

          TUnfold (term,fPatt,_) ->
              undefined

          TCons (nm,terms,pn) -> 
              (TCons (nm,nTerms,pn), fDefns)
            where
              (nTerms,fDefns) = lift_TermList terms

          TDest (nm,terms,pn) ->
              (TDest (nm,nTerms,pn),fDefns)
            where
              (nTerms,fDefns) = lift_TermList terms

          TProd (terms,pn) ->
              (TProd(nTerms,pn), fDefns)
            where
              (nTerms,fDefns) = lift_TermList terms               

          otherwise ->
              (term,[])  

-- ===================================================================

lift_Rec_help :: [(Pattern,Term,PosnPair)] -> 
                 ([(Pattern,Term,PosnPair)],[Defn])
lift_Rec_help []
      = ([],[])

lift_Rec_help ((patt,term,pn):rest)
      = ((patt,newTerm,pn):restList, (tDefns ++ rDefns))
  where
    (newTerm,tDefns)  = lift_Term term  
    (restList,rDefns) = lift_Rec_help rest 


-- ===================================================================

handle_LetWhr :: [LetWhere] -> Term -> (Term,[Defn])
handle_LetWhr lwhrs term 
      = lam_lift_whrDens defns term 
  where 
    --defns  = map (\(LetDefn d) -> d) lwhrs 
    defns = map someFun lwhrs 
    someFun :: LetWhere -> Defn 
    someFun (LetDefn d) = d 
    someFun (LetPatt sthg) = error $ "\n\n" ++ show sthg ++ "\n\n"

-- ===================================================================

handlePattTerms :: [PatternTermPhr] -> ([PatternTermPhr],[Defn])
handlePattTerms []
      = ([],[])

handlePattTerms ((patts,Left term):rest)
      = ((patts,Left newT):newRest,d1 ++ newDefns)
  where
    (newT,d1) = lift_Term term 
    (newRest,newDefns)
              = handlePattTerms rest   

-- ===================================================================

handleFoldPatts :: [FoldPattern] -> ([FoldPattern],[Defn])
handleFoldPatts ((nm,patts,term,pn):rest)
      = ((nm,patts,newT,pn):restfPatt, d1 ++ restDefn)
  where 
    (newT,d1) = lift_Term term 
    (restfPatt,restDefn)
              = handleFoldPatts rest   


-- ===================================================================
-- ===================================================================

lift_req_term :: Term -> Bool 
lift_req_term term
    = case term of  
          TRecord recList ->
              (or.map (\(a,t,p)-> lift_req_term t)) recList
          TCallFun (_,terms,_) ->
              (or.map lift_req_term) terms 

          TLet _ ->
              True 
          
          TIf (t1,t2,t3,_) ->
              (or.map lift_req_term) [t1,t2,t3]

          TCase (term,pattTerms,_) -> 
              (or.map lift_req_term) (term:(map (removeLeft.snd) pattTerms))

          TFold   (term,foldPatts,_) -> 
              (or.map lift_req_term) (term:(map (\(a,b,t,d) -> t) foldPatts))

          TUnfold (term,fPatt,_) ->
              undefined

          TCons (_,terms,_) -> 
              (or.map lift_req_term) terms 

          TDest (_,terms,_) ->
              (or.map lift_req_term) terms

          TProd (terms,_) ->
              (or.map lift_req_term) terms 

          otherwise ->
              False  

removeLeft :: Either a b -> a 
removeLeft (Left l) = l


-- ======================================================================
-- ======================================================================

lam_lift_whrDens :: [Defn] -> Term -> (Term,[Defn])
lam_lift_whrDens defns  term 
      = (finT,finDefns)
  where 
    finDefns= map (addExtraArgs_Defn solEqns) iDefns
    finT    = addExtArgs_Term solEqns newTerm 
    (newTerm,nDefns1) 
            = evalState (freshFNames_Args defns term) (0,0)
    iDefns  = (concat.map get_rid_let) nDefns1 
    setEqns = genSetEqns iDefns  
    solEqns = solveSetEqns setEqns 


lam_lift_del :: [Defn] -> [(Name,[FVar])]
lam_lift_del = (solveSetEqns.genSetEqns)

{-
All the definitons here function definitions. Only things that need to be lambda
lifted are  the defns within the where part of let statements. These
defintions can be mutually recursive.
-}

genSetEqns :: [Defn] -> [SetEqn]
genSetEqns = map genSetEqn  







 

