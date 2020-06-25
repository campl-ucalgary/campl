module TypeInfer.SolveEqns where

import TypeInfer.MPL_AST 
import TypeInfer.Gen_Eqns_CommFuns
import TypeInfer.Unification
import TypeInfer.SolveHelper 

import Control.Monad.State
import Control.Monad.Except
import Data.Either 
import Data.Maybe  
import Data.List 


foldTypeEqn :: ((UniVars,ExistVars) -> [b] -> b ) ->
               ((Type,Type) -> b) -> TypeEqn -> b 
foldTypeEqn fquant fsimple teqn
        = case teqn of 
              TQuant pair ts ->
                  fquant pair (map (foldTypeEqn fquant fsimple) ts) 
              TSimp pair ->
                  fsimple pair     

solveEqns :: [TypeEqn] -> Either ErrorMsg (Log,Package) 
solveEqns eqns = do 
       let 
           eTSol    = solveEqn (head eqns)
           stateVal = runExceptT eTSol
           (eithVal,log)
                    = runState stateVal [""]
       case eithVal of
           Left errormsg -> 
               Left errormsg
           Right package ->
               return (log,package)    


solveEqn :: TypeEqn -> ExceptT ErrorMsg (State Log) Package  
solveEqn  = foldTypeEqn solve_Quant solve_Simp 


-- I need to get  a package out of simple type equations 
solve_Simp :: (Type,Type) ->  ExceptT ErrorMsg (State Log) Package 

solve_Simp (te1,te2) = do 
        let 
           uvars = []
           evars = []
           eithSubst 
                 = match (te1,te2)
           fvars = ( freeVars te1, 
                     freeVars te2 
                   )
        case eithSubst of
            Left errormsg ->
                throwError errormsg
            Right substlist -> do 
                let
                  package 
                       = (fvars,uvars,evars,substlist)
                return package

solve_Quant :: (UniVars,ExistVars) -> [ExceptT ErrorMsg (State Log) Package] -> 
               ExceptT ErrorMsg (State Log) Package 
solve_Quant (uvars,evars) eithList = do 
        oldLog <- lift $ get 
        let 
          -- get all the packages for all the equations in the qunat equation
          stVal   = solve_Quant_intmt eithList
          (eithVal,intLog)
                  = runState stVal oldLog 
        case eithVal of 
            Left errormsg ->
                throwError errormsg
            Right packList -> do 
                let eithTriple = handle_Evars evars packList ([],[])
                case eithTriple of
                    Left eithEmsg ->
                        throwError eithEmsg
                    Right (remEvars,substList,intPacks) ->  
                         case linearize_Cust substList remEvars of 
                             Left emsg1 ->
                                 throwError emsg1
                             Right (tkDupEvars,tkDupsList) -> do 
                                 finPacks <- subst_Packages tkDupsList intPacks 
                                 case combine_packages finPacks of 
                                    Left combemsg -> do 
                                        throwError combemsg
                                    Right finComPack -> do 
                                        let
                                           ((flvars,frvars),intUvars,intEvars,finSubstList)
                                                = finComPack                                      
                                           finUVar 
                                               = nub $ uvars ++ intUvars
                                           finEvar 
                                               = nub $ intEvars ++ tkDupEvars
                                           intNfvars@(ilvars,irvars)
                                               = getNewFreeVars finSubstList   
                                           filvars
                                               = ilvars \\ (finUVar ++ finEvar)
                                           firvars
                                               = irvars \\ (finUVar ++ finEvar)  
                                           newfvars
                                               = (nub filvars,nub firvars)             
                                           ifinPackage
                                               = (newfvars,finUVar,finEvar,finSubstList)
                                        case reduce_Package ifinPackage of 
                                             Left emsg2 ->
                                                 throwError emsg2 

                                             Right upack@(rFreeVars,rUVars,rEVars,rSubstList) -> do
                                                 modify $ \l -> printLog packList upack intLog
                                                 return upack

                                 

                                        



     
                                        
