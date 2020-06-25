{-# LANGUAGE DeriveGeneric #-}

module ToCMPL.PatternComp_Help where

import TypeInfer.MPL_AST 

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint
import Control.Monad.State
import Control.Monad.Except
import Data.List 
import Data.Maybe

import TypeInfer.Gen_Eqns_CommFuns
import TypeInfer.SymTab_DataType
import TypeInfer.SymTab 
import TypeInfer.Gen_Eqns_CommFuns

type Equation = ([Pattern],Term)


data Match =  MatchFun  (FuncName,PosnPair) 
                        [String] [Equation] 
                        Term
              deriving (Eq,Generic,Show)

instance () => Out (Match)

isVarPatt :: Pattern -> Bool 
isVarPatt (VarPattern _) = True 
isVarPatt _              = False 

isConsPatt :: Pattern -> Bool 
isConsPatt (ConsPattern _) = True 
isConsPatt _                 = False  


ruleType :: [Equation] -> Int 
ruleType pattermphrs = do 
    case allVars of 
      True  -> do 
        0 
      False ->
        case allCons of 
          True  ->
            1 
          False ->
            2
   where
      allCons  = (and.map isConsPatt) pattList
      allVars  = (and.map isVarPatt) pattList
      pattList = map (head.fst) pattermphrs

-- ======================================================================
-- ======================================================================

rearrangeEqns :: [Equation] -> (FuncName,PosnPair) -> Term -> 
        ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
                 [[Equation]]

rearrangeEqns eqns fPair@(fname,fPosn) defTerm = do 
    (_,_,_,_,symTab) <- get 
    let 
      allPatts = map (head.fst) eqns
      lenPatts = (length.fst.head) eqns  
      ConsPattern (consName,_,posn)
              = head allPatts
      pConses = nub $ map getConsName allPatts  
      eithVal = lookup_ST (Val_Cons (consName,posn)) symTab
    case eithVal of 
      Left emsg -> 
          throwError emsg
      
      Right retVal -> do 
        pVars <- genNewVarList (lenPatts-1)   

        let 
          paddPatts
                 = map (\x -> VarPattern ("padd_V_" ++ show x,(0,0))) pVars
          ValRet_Cons ((datName,allConses),_,_,_)
                 = retVal 
          miss_cons = allConses \\ pConses
          extr_Cons = pConses \\ allConses
        case extr_Cons == [] of 
            False -> do 
              let
                emsg = 
                  "Error:Extraneous constructors of data type <<" 
                  ++ show datName ++ ">> used in function <<"
                  ++ show fname ++ printPosn fPosn ++ "\n" ++
                  intercalate ", " extr_Cons
              throwError emsg 

            True -> do 
              meqns <- mapM (\x -> missing_Eqn x fPair defTerm paddPatts) miss_cons
              -- prioritise constructors
              let 
                priCons = zip allConses [1..]
                stEQns  = categoriseEqns (eqns ++ meqns) 
                finEqns = evalState stEQns (priCons,1)

              return finEqns


-- This function is going to generate equations for the missing constructors.
-- The term on the right hand side is going to be the default term  
missing_Eqn :: Name -> (FuncName,PosnPair) -> Term -> [Pattern] ->
        ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
                Equation
missing_Eqn consName (fnm,fpn) defTerm paddPatts = do   
    (_,_,_,_,symTab) <- get 
    let 
      eithVal = lookup_ST (Val_Cons (consName,fpn)) symTab
    case eithVal of 
      Left emsg -> 
         throwError emsg 

      Right retVal -> do 
        let 
          ValRet_Cons ((datName,_),_,_,numArgs)
                 = retVal 
        intArgs <- genNewVarList numArgs

        let 
          emsg = "Pattern Error:Constructor <<" ++ consName ++ 
                 ">> of data type <<" ++ show datName ++ 
                 ">> is not present in a pattern in function <<"++ show fnm 
                 ++ ">>" ++ printPosn fpn 

          newVPatts= map (\x ->  VarPattern ("fv" ++ show x,fpn)) intArgs
          consPatt = ConsPattern (consName,newVPatts,fpn)
          finPatt  = consPatt:paddPatts        
          eqn = (finPatt,defTerm)
        return eqn 




{-
Priority list is a pair of constructor names and a priority
assigned to it.

Take a Equation list and group them according to the 
constructors they belong to.  

-}

type PriorityList = [(Name,Int)]

categoriseEqns :: [Equation] -> 
                  State (PriorityList,Int) [[Equation]]
categoriseEqns []   = return []
categoriseEqns eqns = do 
   tripList <- assignPrior eqns
   let 
     gEqns  = groupEqns tripList
     sEqns  = map (sortEqns) gEqns                
     finEqns= map (map getAfromABC) sEqns
   return finEqns

getAfromABC :: (a,b,c) -> a 
getAfromABC (x,y,z) = x

{-
This function will group things that have the
same 2nd argument.This is used with groupWith
function.
-}
boolCond_Group :: (Equation,Int,Int) -> 
                  (Equation,Int,Int) -> Bool
boolCond_Group (_,n1,_) (_,n2,_) 
        = n1 == n2 

{-
This function will sort equations based on its third argument.
This is used with sortBy function.
-}

sortEqns :: [(Equation,Int,Int)] -> [(Equation,Int,Int)]
sortEqns  = sortBy boolCond_Sort

groupEqns :: [(Equation,Int,Int)] -> [[(Equation,Int,Int)] ]
groupEqns eqns = groupBy boolCond_Group fEqns 
     where 
       fEqns = gEqns_help eqns   

gEqns_help :: [(Equation,Int,Int)] -> [(Equation,Int,Int)]
gEqns_help = sortBy group_sort

group_sort ::  (Equation,Int,Int) -> 
               (Equation,Int,Int) -> Ordering
group_sort (_,n1,_) (_,n2,_)
        | n1 > n2  = GT 
        | n1 < n2  = LT 
        | n1 == n2 = EQ  



boolCond_Sort ::  (Equation,Int,Int) -> 
                  (Equation,Int,Int) -> Ordering
boolCond_Sort (_,_,n1) (_,_,n2)
        | n1 > n2  = GT 
        | n1 < n2  = LT 
        | n1 == n2 = EQ  


{-
This function takes a list of equations and
associates a priority with them based on the 
constructor priority. This function also assigns 
a priority to the equations based on their initial 
ordering.


Using these equations can be arranged based on the
priority of their first constructors in the pattern.
This will give back a list of list of equations. Every
list of equations will correspond to a given data 
constructor. In the list of equations within every,
we will arrange the equations on their intial ordering.
-}
assignPrior :: [Equation] -> 
            State (PriorityList,Int) [(Equation,Int,Int)]
assignPrior [] = return []            
assignPrior (eq:eqs) = do 
    (plist,currNum) <- get  

    let 
      mayVal   = lookup (getfstCons eq) plist
      prNum    = fromJust mayVal
      currTrip = (eq,prNum,currNum)

    modify (\(p,cn) -> (p,cn+1)) 
    remTrip <- assignPrior eqs 
    return (currTrip:remTrip) 
     

{-
Take an equation and get the constructor name for the 
first pattern of the pattern list on the left
-- ([Pattern],Either Term [GuardedTerm])
-}
getfstCons :: Equation -> String
getfstCons (patts,_) 
    = getConsName (head patts)  
 

getConsName :: Pattern -> Name 
getConsName (ConsPattern (cname,_,_)) 
    = cname


getLeft :: Either a b -> a
getLeft (Left a) = a


-- ============================================================================
-- ============================================================================

handleGuarded :: [GuardedTerm] -> Term -> 
    ExceptT ErrorMsg (State (Int,TypeThing,Context,ChanContext,SymbolTable))
            Term

handleGuarded [gTerm] defTerm =  
    return (genCase gTerm defTerm)    

handleGuarded (gTerm:gs) defTerm = do 
    fTerm <- handleGuarded gs defTerm
    return (genCase gTerm fTerm)

{-
Term corresponding to True is obtained from the GuardedTerm and the 
False one from the second argument of function.
-}
genCase :: GuardedTerm -> Term -> Term 
genCase (t1,t2) fTerm = lcase
    where
      t1Posn   = getTermPosn t1
      fPosn    = getTermPosn fTerm
      truePatt = ConsPattern ("True",[],t1Posn)
      falsePatt= ConsPattern ("False",[],fPosn)
      pattTerm1= ([truePatt] ,Left t2)
      pattTerm2= ([falsePatt],Left fTerm)
      lcase    = TCase (t1,[pattTerm1,pattTerm2],t1Posn)







