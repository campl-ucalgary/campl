module TypeInfer.SymTab_Helper where

import TypeInfer.MPL_AST
import TypeInfer.SymTab_DataType

-- =========================================================================
-- =========================================================================  
-- =========================================================================

isDataDefn :: Defn -> Bool 
isDataDefn symDefn 
    = case symDefn of
          Data _ ->
              True 
          otherwise ->
              False     

-- *************************************************************************

isCodataDefn :: Defn -> Bool 
isCodataDefn defn 
    = case defn of
          Codata _ ->
              True
          otherwise ->
              False 

-- *************************************************************************

isProtDefn :: Defn -> Bool 
isProtDefn defn 
    = case defn of
          ProtocolDefn _ ->
              True
          otherwise ->
              False 

-- *************************************************************************

isCoProtDefn :: Defn -> Bool 
isCoProtDefn defn
    = case defn of
          CoprotocolDefn _ ->
              True
          otherwise ->
              False 

-- *************************************************************************

isTypeSynDefn :: Defn -> Bool 
isTypeSynDefn defn 
    = case defn of
          TypeSyn _ ->
              True
          otherwise ->
              False 

-- *************************************************************************

isFunDefn :: Defn -> Bool
isFunDefn defn
    = case defn of 
          FunctionDefn _ -> 
              True
          otherwise ->
              False

-- *************************************************************************

isProcDefn :: Defn -> Bool 
isProcDefn defn
    = case defn of 
          ProcessDefn _ ->
              True
          otherwise ->
              False  

-- =========================================================================
-- =========================================================================  
-- =========================================================================

isData :: SymbolDefn -> Bool 
isData symDefn 
    = case symDefn of
          SymData _ ->
              True 
          otherwise ->
              False     

-- *************************************************************************
isCodata :: SymbolDefn -> Bool 
isCodata symDefn 
    = case symDefn of
          SymCodata _ ->
              True 
          otherwise ->
              False  
-- *************************************************************************
isProt :: SymbolDefn -> Bool 
isProt symDefn 
    = case symDefn of
          SymProt _ ->
              True
          otherwise ->
              False 

-- *************************************************************************

isCoProt :: SymbolDefn -> Bool 
isCoProt symDefn 
    = case symDefn of
          SymCoProt _ ->
              True
          otherwise ->
              False 

-- *************************************************************************

isTypeSyn :: SymbolDefn -> Bool 
isTypeSyn symDefn 
    = case symDefn of
          SymTypeSyn _ ->
              True
          otherwise ->
              False 

-- *************************************************************************

isFun :: SymbolDefn -> Bool
isFun symDefn
    = case symDefn of 
          SymFun _ -> 
              True
          otherwise ->
              False    

-- *************************************************************************      

isProc :: SymbolDefn -> Bool 
isProc symDefn
    = case symDefn of 
          SymProc _ ->
              True
          otherwise ->
              False            

-- =========================================================================
-- =========================================================================  
-- =========================================================================

-- the string argument in the output is to tell whether we are 
-- dealing with data or codata
removeCons :: SymbolDefn -> [(Name,ConsVal)]
removeCons (SymData conses) = conses 

-- the string argument in the output is to tell whether we are 
-- dealing with data or codata
removeDest :: SymbolDefn -> [(Name,DestVal)] 
removeDest (SymCodata dests) = dests

-- *************************************************************************      
-- the string argument in the output is to tell whether we are 
-- dealing with data or codata
removeHandCohand :: SymbolDefn -> ([(Name,HandVal)],String) 
removeHandCohand symDefn 
        = case symDefn of 
              SymProt hands ->
                  (hands,"handle")
              SymCoProt cohands ->
                  (cohands,"cohandle") 

-- *************************************************************************      

removeSymFun :: SymbolDefn -> [(FuncName,(FunType,NumArgs))]
removeSymFun (SymFun funs) = funs 

-- *************************************************************************      

removeSymProc :: SymbolDefn -> [(Name,(FunType,(NumArgs,NumArgs,NumArgs)))]
removeSymProc (SymProc procs) = procs  

-- *************************************************************************      

removeSymType :: SymbolDefn -> [(Name,Type)]
removeSymType (SymTypeSyn pairs)
        = pairs 

-- *************************************************************************      




