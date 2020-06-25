module TypeInfer.SolveHelper where 

import TypeInfer.MPL_AST 
import TypeInfer.Unification 
import TypeInfer.Gen_Eqns_CommFuns

import Control.Monad.State
import Control.Monad.Except
import Data.Either 
import Data.List
import Data.Maybe 

{-

Setup - Tquant (univs,exists) innerPackages

Terminology - The Tquant level is being called the outer level and innerPackages level
              is called the inner level in the algorithm below.

1) First reduce every package. 

2) Substitution list is obtained for the existential variables of the outer level equation
   by looking at the free variables of the inner packages.We will also have a few exitentials
   that may not have a corresponding substitution.

3) Then using the substitution obtained from the existential variables from the outer 
   level q each package.

4) In order to see if linearization by a substitution@(x,Term) (for an existential of the outer 
   level equation) can occur look at the free variables of each inner package.
          True  -> Package can be linearized.Linearize with respect to the substitution.
          False -> Move on to the next package

5) As  a result of linearization of packages, there might be substitution available for some existential
   variables of the local package now. Deal with those exitentials.

6) Do this for all the packages such that all packages are in their most reduced form.

7) At the outer leavel combine the packages taking into account the fact that there might be some left
   over existentials from the outer level.

8) This is the package that will be returned for the outer level TQuant equation.

-- Alternatively one can delay the stage through 5 to 8. Combine all the packages post linearization 
   even if a substitution occurs in the package for the local existential. Get rid of the combined
   existentials in one go now.

-- Although this is avoids the localization principle to an extent, this might be more efficient as
   it is not often that existential will get a substituion post linearization, so one would not
   end up wasting a lot of time checking for this for every linearization of  a substitution for
   every package. 


-}

 

solve_Quant_intmt :: [ExceptT ErrorMsg (State Log) Package] ->  
                     State Log (Either ErrorMsg [Package])
solve_Quant_intmt eithList = do 
        lrvals <- solve_Quant_map eithList
        let
           lvals = lefts lrvals
           rvals = rights lrvals  
        case lvals == [] of 
            True  ->
                return $ Right rvals
            False -> 
                return $ Left $ concat lvals  

solve_Quant_map ::[ExceptT ErrorMsg (State Log) Package] ->
                  State Log [Either ErrorMsg Package]  
solve_Quant_map [] 
        = return []
solve_Quant_map (e:es) = do 
        eith  <- solve_Quant_helper e    
        eiths <- solve_Quant_map es 
        return (eith:eiths)

      
solve_Quant_helper :: ExceptT ErrorMsg (State Log) Package ->
                      State Log (Either ErrorMsg Package)
-- old version
-- solve_Quant_helper  = eitherT funError funPackage 
solve_Quant_helper  = runExceptT 

funError :: ErrorMsg -> State Log (Either ErrorMsg Package)
funError emsg = return $ Left emsg

funPackage :: Package -> State Log (Either ErrorMsg Package)
funPackage package = do 
  --modify $ \l ->  subsLog package ++ l 
  return $ Right package 


-- Combine packages 
combine_packages :: [Package] -> Either ErrorMsg Package
combine_packages [(fvars,uvars,evars,substList)] 
        = reduce_Package (fvars,uvars,evars,substList) 
          
combine_packages (((fl1,fr1),u1,e1,s1):((fl2,fr2),u2,e2,s2):rest)
        = combine_packages ((fnew,unew,enew,snew):rest)
    where
      unew = union u1 u2 
      enew = union e1 e2 
      fnew = (
               (union fl1 fl2),
               (union fr1 fr2)  
             )

      snew =  s2 ++ s1

combine_packages sthg = error $ show sthg 

-- ===========================================================================
-- ===========================================================================

printLog :: [Package] -> Package -> Log -> Log 
printLog oPacks newPack oldLog 
        = oldLog ++ eqns ++ intercalate [stars] (map subsLogBefore oPacks) ++ 
          subsLogAfter newPack ++ ["\n",equalS,equalS,"\n"] 
    where
      eqns = ["Quant Equation ::"]

              

subsLogAfter :: Package -> [String]
subsLogAfter (nfvar,nuvar,nevar,nsubstList)
           = [ "\n",stars,"\t\tAFTER",stars,
               "\nFree Vars :\n",show nfvar,
               "\nUniv Vars :\n",show nuvar,
               "\nExistVars :\n",show nevar,
               "\nSubstitution :\n"
             ] ++ [" [","\t" ++ (intercalate ",\n\t" .map show) nsubstList," ]"] 

subsLog :: Package -> [String]
subsLog (nfvar,nuvar,nevar,nsubstList)
           = [ "\n",stars,
               "\nFree Vars :\n",show nfvar,
               "\nUniv Vars :\n",show nuvar,
               "\nExistVars :\n",show nevar,
               "\nSubstitution :\n"
             ] ++ [" [", "\t" ++ (intercalate ",\n\t" .map show) nsubstList," ]"] 


subsLogBefore :: Package -> [String]
subsLogBefore (ofvar,ouvar,oevar,osubstList)
           = case False of 
                 True  ->
                      []
                 False -> 
                      [ "\n",stars ,"\t\tBEFORE",stars,
                         "\nFree Vars :\n",show ofvar,
                         "\nUniv Vars :\n",show ouvar,
                         "\nExistVars :\n",show oevar,
                         "\nSubstitution :\n"
                       ] ++ [" [","\t" ++ (intercalate ",\n\t" .map show) osubstList," ]"]

-- ===========================================================================
-- ===========================================================================
subst_Packages :: SubstList -> [Package] ->
                  ExceptT ErrorMsg (State Log) [Package]
subst_Packages [] packs
        = return packs  
subst_Packages (subst:rest) packs = do 
        newPacks <- subst_Package subst packs []
        subst_Packages rest newPacks


subst_Package :: Subst -> [Package] -> [Package] -> 
                 ExceptT ErrorMsg (State Log) [Package]
subst_Package _ [] shunt_packs = return shunt_packs

subst_Package subst@(s,texpr) (p:ps) shunt_packs = do 
        let 
          ((flvars,frvars),uvars,evars,substList)  
              = p 
        case elem s (flvars++frvars) of 
            True  ->
                case coalesce_Evar (subst,substList) of
                    Left errormsg  ->
                        throwError errormsg  
                    Right newSList -> do 
                        let 
                          newfreeVars
                                 = getNewFreeVars newSList 
                          intmtP = (newfreeVars,uvars,evars,newSList)
                          newP   = reduce_Package intmtP 
                        case newP of 
                            Left redemsg  ->
                                throwError redemsg
                            Right redPack ->   
                                subst_Package subst ps (redPack:shunt_packs)
            False ->
                subst_Package subst ps (p:shunt_packs)
 

getNewFreeVars :: SubstList -> (FreeVars,FreeVars)
getNewFreeVars slist 
        = (arg1,arg2)
    where
      arg1  = map fst slist    
      arg2 = (nub.concat.map (freeVars.snd)) slist
      

getRight :: Either a b -> b 
getRight eith 
        = case eith of 
              Left sthg ->
                  error $ "error in getRight Function(SolveEqns module)"
              Right val -> 
                  val


-- Argument is the package to be reduced.
reduce_Package :: Package -> Either ErrorMsg Package 
-- final free vars is always collected during the base case of 
-- commonElems == []. In other cases , it doesn't matter.
reduce_Package (dcareFVars,uvars,evars,slistRe) = do  
        let
          newSubstList= remove_Reciproc slistRe
          -- see if there are some common element between
          -- newSubsVars and evars
          dupnewSubsVars
                      = map fst newSubstList
          newSubsVars = nub dupnewSubsVars
          commonElems = intersect evars newSubsVars
          remEvars    = evars \\ commonElems  
        case commonElems == [] of
            True -> do 
              case uvars == [] of 
                  True -> do 
                      let 
                        totEUVars= uvars ++ evars 
                        nFrvars  = (nub.concat.map (freeVars.snd)) newSubstList 
                        intUVars = newSubsVars\\totEUVars
                        intEVars = nFrvars\\totEUVars                        
                        newPackage
                                 = (
                                     (intUVars,intEVars),uvars,evars,
                                     nub newSubstList 
                                   )
                      return newPackage

                  False -> 
                      handle_UVars (dcareFVars,uvars,evars,slistRe)

            False -> do      
                -- find out substitution corresponding to the common elems
                -- common subs are the subs corresponding to the evars.
                finSubstList <- commonSubsFun (nub newSubstList) commonElems 
                reduce_Package  (
                                  ([],[]),uvars,remEvars,
                                  nub finSubstList
                                ) 

-- ==========================================================================
-- ==========================================================================

                                 
handle_UVars :: Package ->  Either ErrorMsg Package
handle_UVars (freeVars,uvars,evars,substList) 
    = case uvars == [] of 
        True  ->  
          reduce_Package (([],[]),[],evars,substList)

        False -> do 
          let 
            (u:us)   = uvars 
            uvarsmsg = "Inferred type and given types don't match"
          case lookup u substList of
            Just texpr -> 
              case texpr of 
                TypeVarInt v ->
                  case u == v of
                    True -> do 
                      let 
                        newList 
                          = (delete (u,TypeVarInt u) substList)
                        newPack 
                          = (([],[]),us,evars,newList)
                      handle_UVars newPack
                    False -> 
                      case elem v evars of 
                          True  -> do 
                            let 
                              newSList
                                  = (v,TypeVarInt u):
                                    (delete (u,TypeVarInt v) substList)
                              newPack
                                  = (([],[]),uvars,evars,newSList)      
                            reduce_Package newPack          

                              
                          False ->
                            Left uvarsmsg
                otherwise ->
                    Left uvarsmsg
            Nothing -> 
                 handle_UVars (([],[]),us,evars,substList) 

-- ==========================================================================
-- ==========================================================================
commonSubsFun :: SubstList -> ExistVars -> Either ErrorMsg SubstList
commonSubsFun slist [] 
        = return slist
commonSubsFun slist (e:es) = do
        let
          mexpr = lookup e slist  
           -- we know for a fact that we are going to get e in slist because 
           -- it is by design hence no case for Nothing
        case mexpr of 
            Just expr -> 
                case (check (expr,TypeVarInt e)) of 
                    Left errormsg ->
                        Left errormsg
                    Right subs -> do 
                        let 
                          remSList = delete (e,expr) slist
                        coaEList <- coalesce_Evar (head subs,remSList) 
                        commonSubsFun coaEList es 
            Nothing -> 
                Left $ "Can't Find existential variable <<" 
                       ++ show e ++ ">> in \n" ++ show slist                

-- ==========================================================================
-- ==========================================================================
-- This function takes a substList and uses it to do replacement for the existentials.
--  
reduce_Package_helper :: SubstList -> SubstList -> Either ErrorMsg SubstList
reduce_Package_helper [] slist = return slist  
reduce_Package_helper (s:ss) substList = do 
        case coalesce_Evar (s,substList) of 
            Left errormsg -> 
                Left errormsg 
            Right nsubstList ->
                reduce_Package_helper ss nsubstList     

-- ===========================================================================
-- ===========================================================================

-- Output is a list of Existential variables that can't be eliminated from
-- the original existential list, a list of substitutions for existentials
-- that can be eliminated and a list of packages that don't have the afore
-- mentioned substitutions in them.

handle_Evars :: ExistVars -> [Package] -> (ExistVars,SubstList) ->
               Either ErrorMsg (ExistVars,SubstList,[Package])
 
handle_Evars [] finPacks (finShuntEvar,finSList) = do 
        let
          (fEvar,fSList) = check_Reciproc finShuntEvar finSList
        return (fEvar,fSList,finPacks)
                                              
handle_Evars (e:es) packs (shuntEvar,slist) = do 
        let 
          (newSEVars,newSList) 
                     = check_Reciproc shuntEvar slist
          eithVal = check_package e packs []  
        case eithVal of
              Right (maySubst,newPacks) -> 
                  case maySubst of 
                      Just subst ->                               
                          handle_Evars es newPacks (newSEVars,subst:newSList)
                      Nothing -> 
                          handle_Evars es newPacks (e:newSEVars,newSList) 
              Left errormsg ->
                  Left errormsg           
      




-- This function takes an existential variable and a list of packages.
-- If there is a substitution in any package then return the subtitution along 
-- with the new package list (the package in which the substitution is found 
-- is changed by removing that substitution). If it is not present in any 
-- substitution then simply return Nothing
check_package :: ExistVar -> [Package] ->  [Package] ->
                 Either ErrorMsg (Maybe Subst,[Package])
                 
check_package evar [] shunt_packs
        = Right (Nothing,shunt_packs) 

check_package evar (p:ps) shunt_packs = do           
        let ((flvars,frvars),uvars,evars,substlist) = p
        case elem evar flvars of 
              True  -> 
                  case myLookup evar substlist evars of 
                      Just texpr -> do 
                           case TypeVarInt evar /= texpr of 
                               True -> do  
                                         let
                                           subs  = (evar,texpr) 
                                           newSList
                                                 = delete subs substlist        
                                           newFrVars 
                                                 = getNewFreeVars newSList     
                                           newPackageP
                                                 = (newFrVars,uvars,evars,newSList) 
                                         return$ ( Just subs,
                                                       (shunt_packs++(newPackageP:ps))
                                                 )
 
                               -- stopping evars to be removed because of (TvarInt 5,TvarInt5)    
                               False -> do 
                                    let 
                                      subs  = (evar,texpr) 
                                      newSList
                                           = delete subs substlist  
                                      newFlvars = delete evar flvars
                                      newFrvars = delete evar frvars 
                                      newPack   = ((newFlvars,newFrvars),uvars,evars,newSList)
                                    check_package evar ps (newPack:shunt_packs)   

                      Nothing -> do 
                          check_package evar ps (p:shunt_packs)
              False ->   
                  check_package evar ps (p:shunt_packs)
        

-- ===========================================================================
-- ===========================================================================

-- This Function is a normal lookup function plus the condition that 
myLookup :: ExistVar -> SubstList -> ExistVars -> Maybe Type 
myLookup e slist evars 
        = case lookup e slist of 
              Nothing   -> 
                  Nothing
              Just expr -> 
                  case intersect (freeVars expr) evars /= [] of 
                      True -> 
                          myLookup e (delete (e,expr) slist) evars 
                      False ->
                          Just expr     


