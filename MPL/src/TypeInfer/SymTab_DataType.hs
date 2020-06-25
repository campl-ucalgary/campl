module TypeInfer.SymTab_DataType where

import TypeInfer.MPL_AST

type ConsVal = ((DataName,[Name]),FunType,(FunType,FunType),Int) -- Second one 
-- 
type DestVal = ((DataName,[Name]),FunType,Int)
-- ((data Name,allconstructors) ,constructor type,
-- fold Type of constructor,noumber of constructor args)
type HandVal = (ProtName,FunType)

data SymbolDefn  =   SymData    [(Name,ConsVal)] --first is the constructor name)
                   | SymCodata  [(Name,DestVal)] 
                   | SymProt    [(Name,HandVal)]
                   | SymCoProt  [(Name,HandVal)]
                   | SymTypeSyn [(Name,Type)]
                   | SymFun     [(FuncName,(FunType,NumArgs))]
                   | SymProc    [(Name,(FunType,(NumArgs,NumArgs,NumArgs)))]
                  deriving (Eq,Show)

data ScopeType =  OldScope
                | NewScope 
                deriving (Eq,Show) 

data Polarity     =  In 
                   | Out
                    deriving (Eq,Show) 

type ScopeSymbols = [SymbolDefn]
type EndFlag      = Int 
type SymbolTable  = [ScopeSymbols]
type Context      = [ScopeContext] 
type ScopeContext = [(String,Type)]
type ChanContext  = [(String,(Polarity,Type))]
-- the second last protocol 



-- These are the things that will be looked up in the symbol table.
data ValLookup =    Val_Cons    (Name,PosnPair)
                  | Val_Dest    (Name,PosnPair) 
                  | Val_Prot    (Name,PosnPair)
                  | Val_Coprot  (Name,PosnPair)
                  | Val_TypeSyn (Name,PosnPair) 
                  | Val_Fun     (FuncName,PosnPair) 
                  | Val_Proc    (Name,PosnPair)
                 deriving (Eq,Show)

-- The return values from the symbol table.
data ValRet =     ValRet_Cons   ((DataName,[Name]),FunType,(FunType,FunType),NumArgs)
                | ValRet_Dest   ((DataName,[Name]),FunType,NumArgs)
                | ValRet_Prot   (ProtName,FunType)
                | ValRet_Coprot (ProtName,FunType)
                | ValRet_TypeSyn Type 
                | ValRet_Fun    (FunType,NumArgs)
                | ValRet_Proc   (FunType,(NumArgs,NumArgs,NumArgs))
               deriving (Eq,Show) 


