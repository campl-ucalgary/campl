module TypeInfer.PreProcess_Let where 

import TypeInfer.Gen_Eqns_CommFuns
import TypeInfer.MPL_AST
import Data.List 

preprocessBefTyping :: MPL -> MPL 
preprocessBefTyping = map preproc_Stmt  

preproc_Stmt :: Stmt -> Stmt 
preproc_Stmt stmt 
    = case stmt of 
        DefnStmt (defns,stmts,pn) -> 
          DefnStmt ( 
                     map preproc_Defn defns,
                     map preproc_Stmt stmts,pn 
                   )
        otherwise -> 
          stmt 

preproc_Defn :: Defn -> Defn 
preproc_Defn defn 
    = case defn of 
        FunctionDefn (fnm,fT,pattsList,pn) ->
          FunctionDefn (fnm,fT,map preproc_pattTerm pattsList,pn)

        otherwise -> 
          defn 


preproc_pattTerm :: (PatternTermPhr,PosnPair) -> (PatternTermPhr,PosnPair)
preproc_pattTerm ((patts,eithTerm),pn)
    = case eithTerm of 
          Left term -> 
               ((patts,Left newTerm),pn)
            where 
               newTerm = pattTerm_help term

          Right gTerms -> 
               ((patts,Right finGTerms),pn)
            where 
              allSTerms = map snd gTerms
              allFTerms = map fst gTerms
              newSTerms = map pattTerm_help allSTerms
              finGTerms = zip allFTerms newSTerms          



pattTerm_help :: Term -> Term 
pattTerm_help term
    = case term of 
          TLet (lTerm,lWhrs,pn) ->
                handleLet_help letPatts lTerm newLetDefns pn
             where 
               letDefn = filter isFunDefnLWhr lWhrs
               newLetDefns
                       = map (\(LetDefn d) -> LetDefn (preproc_Defn d)) letDefn
               letPatts= lWhrs \\ letDefn
          
          TCase (term,pattTerms,pn) -> 
              TCase (term,finCasePatts,pn) 
            where
              pattsPosn    = map (\x -> (x,(0,0))) pattTerms
              finCasePatts = map fst (map preproc_pattTerm pattsPosn) 

          TCons (cname,terms,pn) ->
              TCons (cname,map pattTerm_help terms,pn) 

          otherwise ->  
                term


isFunDefnLWhr :: LetWhere -> Bool 
isFunDefnLWhr (LetDefn _) = True 
isFunDefnLWhr _           = False 

-- ========================================================================
-- ========================================================================

{-
Important assumption - All the variable assignments should occur before 
the defintions
-}
handleLet_help :: [LetWhere] -> Term -> [LetWhere] -> PosnPair ->  Term  

handleLet_help [] letTerm letDefn posn
      = TLet (letTerm, letDefn,posn)  

handleLet_help ret@(lwhr:rest) origLTerm letDefn pn 
      = let 
          LetPatt (patt,letTerm) 
                = lwhr 
          VarPattern pair
                = patt 
          subst = (TVar pair,letTerm)
            -- substitute in the let term 
          newLTerm = subsInTerm subst origLTerm   
            -- substitute in the other where clause underneath the 
            -- current one 
          newRest = map (substLetWhr subst) rest
          newDefn = map (substInDefn subst) letDefn
        in 
          handleLet_help newRest newLTerm newDefn pn 


