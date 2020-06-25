module TypeInfer.Unification  where

import TypeInfer.MPL_AST 
import TypeInfer.Gen_Eqns_CommFuns
import Data.Either 
import Data.List 

-- here the second expression is always a TVar
check :: (Type,Type) -> Either ErrorMsg SubstList
check (texpr1,texpr2)
        = case (texpr1==texpr2) of
              True  -> do
                   return []

              False ->
                   case doOccursCheck (texpr1,texpr2) of
                       True  -> do 
                           let 
                              TypeVarInt var2 = texpr2
                           return [(var2,texpr1)]
                       False -> do 
                           let 
                             (t1,t2) = showErrorFun texpr1 texpr2
                           Left $ "Fails Occurs Check:" ++
                                  "Can't match given type <<" ++ showError t2
                                   ++ ">> with actual type <<" ++ printTypePn t1 

-- here too the second expression is a TVar 
-- return True if occurs check is passed
doOccursCheck :: (Type,Type) -> Bool 
doOccursCheck (te1,TypeVarInt x)
    = case te1 of
          TypeVarInt y ->
              if x == y then False else True

          TypeFun (texprs,texpr,_) ->
              (and.map (\e -> doOccursCheck (e,TypeVarInt x))) (texpr:texprs)

          TypeDataType (_,dins,_) ->
              (and.map (\e -> doOccursCheck (e,TypeVarInt x))) dins

          TypeCodataType (_,dins,_) ->
              (and.map (\e -> doOccursCheck (e,TypeVarInt x))) dins

          TypeProd (prods,_) ->
              (and.map (\e -> doOccursCheck (e,TypeVarInt x))) prods  

          Get(t1,t2,_) ->
              (and.map (\e -> doOccursCheck (e,TypeVarInt x))) [t1,t2]

          Put(t1,t2,_) ->
             (and.map (\e -> doOccursCheck (e,TypeVarInt x))) [t1,t2]

          Neg (t,_) ->
             doOccursCheck (t,TypeVarInt x) 

          ProtNamed (_,types,_) ->
             (and.map (\e -> doOccursCheck (e,TypeVarInt x))) types 

          CoProtNamed (_,types,_) ->
             (and.map (\e -> doOccursCheck (e,TypeVarInt x))) types 
          
          ProtTensor (t1,t2,_) -> 
             (and.map (\e -> doOccursCheck (e,TypeVarInt x))) [t1,t2]

          ProtPar (t1,t2,_) ->
             (and.map (\e -> doOccursCheck (e,TypeVarInt x))) [t1,t2]

          ProtProc (sins,its,ots,_) ->
             (and.map (\e -> doOccursCheck (e,TypeVarInt x))) (sins++its++ots)

          otherwise ->
              True 
                     
               
match :: (Type,Type) -> Either ErrorMsg SubstList
match (t1,t2)
        = case (t1,t2) of   

              (TypeVarInt x,texpr) -> 
                  check (texpr,TypeVarInt x)

              (texpr,TypeVarInt x) ->
                  check (texpr,TypeVarInt x) 

              (fexpr,gexpr)  -> 
                  case matchStructure (fexpr,gexpr) of
                      Right ziplist  -> do 
                           let eithList = map match ziplist
                               (ls,rs)  = partitionEithers eithList
                           case ls == [] of
                               True ->     
                                   return $ concat rs 
                               False -> 
                                    Left $ (concat.map (\x ->  x ++ "\n")) ls       
                      Left emsg -> 
                           Left emsg


{-
See if the structures can be unified.
if yes you are in luck otherwise time to get debugging buddy.
-}
matchStructure :: (Type,Type) -> Either ErrorMsg [(Type,Type)]
matchStructure (texpr1,texpr2)
        = case (texpr1,texpr2) of 
              (Unit _ ,Unit _) ->
                   return []

              (Neg (t1,_),Neg (t2,_)) ->
                   return [(t1,t2)]

              (TypeFun (ins1,out1,posn1),TypeFun (ins2,out2,posn2)) ->
                   case length ins1 == length ins2 of
                       True -> 
                           return $ zip (out1:ins1) (out2:ins2)
                       False -> do 
                           let 
                              emsg = "\nFunction called with incorrect number of arguments\n"
                                      ++ matchError (texpr1,texpr2) 
                           Left emsg        

              (TypeDataType (dname1,dins1,dposn1),TypeDataType (dname2,dins2,dposn2)) ->
                   case dname1 == dname2 of 
                       True  -> do 
                           return $ zip dins1 dins2
                       False ->  
                           Left $ matchError (texpr1,texpr2)   

              (TypeCodataType (cname1,dins1,dposn1),TypeCodataType (cname2,dins2,dposn2)) ->
                   case cname1 == cname2 of 
                       True  ->  
                           return $ zip dins1 dins2
                       False ->  
                           Left $ matchError (texpr1,texpr2) 

              (TypeProd (prods1,posn1),TypeProd (prods2,posn2)) -> do 
                   let 
                     len1 = length prods1
                     len2 = length prods2
                   case len1 == len2 of 
                       True  ->  
                           return $ zip prods1 prods2
                       False -> do 
                           let 
                             emsg = "Expecting a tuple of length " ++ show len1 
                                      ++ ", <<" ++ showError texpr1 ++ 
                                     ">> instead got one with length " ++ 
                                     show len2 ++ "\n" ++ matchError (texpr1,texpr2)
                           Left emsg  

              (TypeConst (bt1,posn1),TypeConst (bt2,posn2)) ->
                   case bt1 == bt2 of 
                       True  -> 
                           return [] 
                       False -> do
                           Left $ matchError (texpr1,texpr2)  

              (Get (t1,t2,_),Get (t3,t4,_)) ->
                  return [(t1,t3),(t2,t4)] 

              (Put (t1,t2,_),Put (t3,t4,_)) ->
                  return [(t1,t3),(t2,t4)]  

              (TopBot _,TopBot _) ->
                  return []

              (ProtNamed (pnm1,types1,_),ProtNamed (pnm2,types2,_)) -> do 
                  case pnm1 == pnm2 of 
                      True  -> 
                          return $ zip types1 types2 
                      False -> 
                          Left $ matchError (texpr1,texpr2)

              (CoProtNamed (pnm1,types1,_),CoProtNamed (pnm2,types2,_)) -> do 
                  case pnm1 == pnm2 of 
                      True  -> 
                          return $ zip types1 types2 
                      False -> 
                          Left $ matchError (texpr1,texpr2)

              (ProtTensor (t1,t2,_),ProtTensor (t3,t4,_)) ->
                  return [(t1,t3),(t2,t4)]  

              (ProtPar (t1,t2,_),ProtPar (t3,t4,_)) ->
                  return [(t1,t3),(t2,t4)]  

              (ProtProc (s1,in1,out1,pn1),ProtProc (s2,in2,out2,pn2)) -> do 
                  let 
                    boolCond = (length s1   == length s2,
                                length in1  == length in2,
                                length out1 == length out2)
                  case boolCond of 
                      (True,True,True) ->
                          return $ zip (s1++in1++out1) 
                                       (s2 ++ in2 ++ out2)
                      _ ->  
                          Left $ procError boolCond  
  
              otherwise -> do 
                  Left $ matchError (texpr1,texpr2)    

procError :: (Bool,Bool,Bool) -> String
procError bcond 
    = "\nCan't unify two process types with unequal numbers of " ++
      procError_helper bcond ++ "\n"

procError_helper :: (Bool,Bool,Bool) ->   String
procError_helper boolCond 
    = case boolCond of 
          (False,False,False) ->
              "sequential args,input channel and output channels."
          (False,True,True) ->
              "sequential args."
          (True,False,True) ->
              "input channels."
          (True,True,False) ->
              "output channels."   
          (False,False,True) ->
              "sequential args and input channels." 
          (False,True,False) ->
              "sequential args and output channels." 
          (True,False,False) -> 
              "input channels and output channels."

matchError :: (Type,Type) -> ErrorMsg
matchError (texpr1,texpr2)
        = "\nExpected type <<" ++ showError texpr1 
           ++ ">> instead got <<" ++ showError texpr2 ++ ">>" ++
            (printPosn.getTypePosn.select_TExpr texpr1) texpr2 


select_TExpr :: Type -> Type -> Type 
select_TExpr te1 te2 =
        case l1-l2 >= 0 of 
            True  -> te1 
            False -> te2   
    where
      (l1,c1) = getTypePosn te1 
      (l2,c2) = getTypePosn te2 


showError :: Type -> String
showError t 
        = case intTypeToStrType iType of
              Left emsg ->
                  show t
              Right strType -> 
                  case newType of
                      TypeProd _ -> show (show newType)
                      otherwise  -> show newType  
                where
                  StrFType (_,newType)
                        = strType  
            where
              iType = IntFType (freeVars t,t)  
            
showErrorFun :: Type -> Type -> (Type,Type)
showErrorFun t1 t2  
        = case intTypeToStrType iType of
              Left emsg ->
                  (t1,t2)
              Right strType -> 
                  ((head ins),out)
                where  
                  StrFType (_,newType)
                        = strType  
                  TypeFun (ins,out,pn)
                        = newType      
    where
       tFun  = TypeFun ([t1],t2,(0,0))
       iType = IntFType (freeVars t1 ++ freeVars t2,tFun)



printTypePn :: Type -> String 
printTypePn sType 
        = showError sType ++ ">> " ++ printPosn pn 
    where
      pn = getTypePosn sType



coalesce ::  (Subst,SubstList) -> Either ErrorMsg SubstList
coalesce (subst,slist) = do 
        newSubstList <- coalesce_Evar (subst,slist)
        return newSubstList  



{-
This function does the following .
It take a substitution and a substitution list and does the following for each element 
of the substlist.
if the left variable of the subst and the element of substList matches ,
then the right exprs are matched to get a substitution list. otherwise
substitution is done in the lement of the substList with the right hand side of subst.
-}

coalesce_Evar :: (Subst,SubstList) -> Either ErrorMsg SubstList
coalesce_Evar (_,[]) = return []
coalesce_Evar (subst,s:ss) = do 
        newS  <- coalesce_helper subst s 
        newSS <- coalesce_Evar (subst,ss) 
        return $ (newS ++ newSS ) 
         

coalesce_helper :: Subst -> Subst -> Either ErrorMsg SubstList
coalesce_helper (x,t) (y,s)
        = case x == y of
              True -> 
                  match (t,s)
              False -> do 
                  let
                    subs_s = substInTExpr (x,t) s  
                  check (subs_s,TypeVarInt y)
                       

substInTExpr:: Subst -> Type -> Type
substInTExpr (x,t) texpr 
        = case texpr of 
              TypeVarInt y ->
                  if x == y then t else texpr

              TypeFun (tins,tout,posn) ->
                  TypeFun
                      (
                        (map (substInTExpr (x,t)) tins),
                        (substInTExpr (x,t) tout),posn 
                      )  
              
              TypeDataType (dname,dins,dposn) ->
                  TypeDataType 
                      (dname,map (substInTExpr (x,t)) dins,dposn)

              TypeCodataType (cname,dins,dposn) ->
                  TypeCodataType 
                      (cname,map (substInTExpr (x,t)) dins,dposn)

              TypeProd (prods,pposn) -> 
                  TypeProd 
                     (
                      map (substInTExpr (x,t)) prods ,
                      pposn
                     )

              Get (t1,t2,pn) ->
                  Get 
                      ( 
                       substInTExpr (x,t) t1,
                       substInTExpr (x,t) t2,pn
                      )         

              Put (t1,t2,pn) ->
                  Put 
                      ( 
                       substInTExpr (x,t) t1,
                       substInTExpr (x,t) t2,pn
                      ) 

              Neg (nt,pn) ->
                  case nt of 
                      Neg (nnt,pn) ->  
                          substInTExpr (x,t) nnt 
                      otherwise ->  
                          Neg (substInTExpr (x,t) nt,pn)
                    

              ProtNamed (nm,ts,pn) ->
                  ProtNamed 
                      (nm,map (substInTExpr (x, t)) ts,pn)

              CoProtNamed (nm,ts,pn) ->
                  CoProtNamed 
                      (nm,map (substInTExpr (x,t)) ts,pn)

              ProtTensor (t1,t2,pn) ->
                  ProtTensor 
                      ( 
                       substInTExpr (x,t) t1,
                       substInTExpr (x,t) t2,pn
                      ) 
              ProtPar (t1,t2,pn) ->
                  ProtPar
                      ( 
                       substInTExpr (x,t) t1,
                       substInTExpr (x,t) t2,pn
                      ) 
              ProtProc (sts,its,ots,pn) ->
                  ProtProc 
                      (
                       map (substInTExpr (x,t)) sts,
                       map (substInTExpr (x,t)) its,
                       map (substInTExpr (x,t)) ots,
                       pn 
                      )
              otherwise ->
                  texpr 

linearize_Cust :: SubstList -> ExistVars -> Either ErrorMsg (ExistVars,SubstList)
linearize_Cust [] evars
        = return (evars,[])  
linearize_Cust (s:ss) evars = do 
        intmt1 <- coalesce (s,ss)
        let 
          (newEvars,intmt11) = check_Reciproc evars intmt1
        (finEvars,intmt2) <- linearize_Cust intmt11 newEvars
        return $ (finEvars,(s:intmt2)) 


-- =============================================================
-- =============================================================
-- checking for things like (2,TVarInt 4) and (4,TVarInt 2)

remove_Reciproc :: SubstList -> SubstList
remove_Reciproc slist = snd (check_Reciproc [] slist)

check_Reciproc :: ExistVars -> SubstList -> (ExistVars,SubstList)
check_Reciproc evars slist
        = (nub finEvars,nub $ finSimp++notsimp)
     where    
       simp    = filter checkSimp slist     
       notsimp = slist \\ simp
       (finEvars,finSimp)
               = check_Reciproc_help simp evars []  
 

check_Reciproc_help :: SubstList ->  ExistVars -> SubstList -> (ExistVars,SubstList)
check_Reciproc_help [] evars finSList 
        = (evars,finSList)
check_Reciproc_help ((x,TypeVarInt y):rest) evars shuntList 
        = case elem (y,TypeVarInt x) rest of 
              True  -> 
                  check_Reciproc_help newRest newEvars ((x,TypeVarInt y):shuntList)
                where
                  newRest  = delete (y,TypeVarInt x) rest   
                  newEvars = (y:evars)
              False -> 
                  check_Reciproc_help rest evars ((x,TypeVarInt y):shuntList)


checkSimp :: Subst -> Bool
checkSimp (x,TypeVarInt y) = True
checkSimp _                = False  

-- =============================================================
-- =============================================================
            
