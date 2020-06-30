{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
module CMPLAST where 
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

import Data.Functor.Foldable.TH

import AMPLTypes 

{-
type Name                 = String  
type Argument             = (String,RowColPos)
type Channel              = Ident


type Handler              = (Event_Handle,Process )
type Event_Handle         = (Ident,Ident)
type Struct_Handle        = (Ident,Ident,[Ident])
type Struct_Name          = (Ident,Ident)
type ForkPart             = (Channel,[Channel],Process)
type PlugPart             = ([Channel],Process)
type Include_Defns        = Defn 
type Data_Defns           = [Defn]
type Codata_Defns         = [Defn]
type Protocol_Defns       = [Defn]
type CoProtocol_Defns     = [Defn]
type Function_Defns       = [Defn]
type Process_Defns        = [Defn]
type MainRun_Defn         = Defn 
type ProtocolClause       = (Ident,[Ident]) -- protocol name & list of handles 
type Process              = [ProcessCommand ]
type ProcessPhrase_pcase  = (Struct_Handle ,Process)
type ProcessPhrase_hcase  = (Event_Handle,Process )
type PatternDef           = (Struct_Handle ,[Term] )
type DataClause           = (Ident,[Constructor])
type Constructor          = (Ident,(Int,RowColPos))

data Defn                 =   Includes [String]    
                            | Data   (RowColPos, DataClause)
                            | Codata (RowColPos, DataClause)
                            | Protocol (RowColPos, ProtocolClause)
                            | CoProtocol (RowColPos, ProtocolClause)
                            | Function (RowColPos, FuncName,[Argument],Term) 
                            | Process (RowColPos,Ident,[Argument],[Channel],[Channel],[ProcessCommand])
                            | MainRun (RowColPos,[Channel],[Channel],[ProcessCommand]) 
                            deriving (Eq,Show,Read,Generic,Out)
                            -}


data BuiltInOps = 
    AddInt
    | SubInt
    | MulInt 
    | DivInt
    | ModInt 

    | EqInt
    | LeqInt 

    | EqChar
    | LeqChar

    | AndBool
    | OrBool
    {-
    | Eq_S
    | Leq_S
    | Concat_S Int 
    | Unstring_S 
    | ToStr
    | ToInt
    | Append
    | Or_B
    | And_B
    -}
  deriving (Eq,Show,Read,Generic,Out)

{-
data Func                 =    Add_I
                             | Sub_I
                             | Mul_I 
                             | DivQ_I
                             | DivR_I 
                             | Eq_I
                             | Leq_I 
                             | Eq_C
                             | Leq_C
                             | Eq_S
                             | Leq_S
                             | Concat_S Int 
                             | Unstring_S 
                             | ToStr
                             | ToInt
                             | Append
                             | Or_B
                             | And_B
                             deriving  (Eq,Show,Read,Generic,Out)
-}


data Expr = 
    ECall String [Expr]
    | EOp BuiltInOps Expr Expr
    | ECons ((String,String), [Expr])
        -- data name, constructor, args
    | EDest ((String,String), Expr)
        -- codata name, destructor, args, var

    | ECase Expr [((String,String),[String], Expr)]
        -- case on, [(base, sub), args, val]
    | EIf Expr (Expr, Expr)

    | EVar String

    | EConstChar Char
    | EConstInt Int
    | EString String

    | ERecord [((String,String),[String], Expr)]

    | EProduct [Expr]
    | EProductElem (Word, Expr)

    | EError String
  deriving (Eq,Show,Read,Generic,Out)

{-
data Term =
    TCall   (FuncName,[Term]) 
                            | TCons   (Struct_Name,[Term]) 
                            | TDest   (Ident,Struct_Name,[Term])

                            | TCase   (Term ,[PatternDef],RowColPos) 
                                -- last posn , beginning of case keyword

                            | TVar    (String,RowColPos)  
                            | TConstS (String,RowColPos)
                            | TConstC (Char,RowColPos)
                            | TConstI (Int,RowColPos)   
                            | TRec    ([(Struct_Handle,Term)],RowColPos)
                            | TProd   [Term]
                            | TProdElem (Int,Term,RowColPos)
                            | TError String 
                            deriving  (Eq,Show,Read,Generic,Out)
                            -}


data ProcessCommand =
    PRun (String, ([Expr], ([String], [String])))
        -- name, (seq vars, inchs, outchs)
    | PClose String
    | PHalt [String]

    | PHCase String [((String, String), [ProcessCommand])]
        -- channel, ((namebase, subname), commands) 
    | PHPut (String, String) String
        -- (namebase, subname), channel

    | PGet String String
    | PPut Expr String
        -- value on channel

    | PSplit String (String, String)
        -- channel into channels..

    | PFork String 
        ((String, [String]),[ProcessCommand])
        ((String, [String]),[ProcessCommand])
        -- fork a as (a1 with ... commands) ..  (a2 with ... commands)

    | PPlug [String] 
            ([String], [ProcessCommand])
            ([String], [ProcessCommand])

    | PId (String, String)
        -- channel |=| channel

    | PCase Expr [((String,String),[String], [ProcessCommand])]
        -- casing is surprsingly allowed in concurrent instructions

    | PIf Expr ([ProcessCommand], [ProcessCommand])
        -- casing is surprsingly allowed in concurrent instructions

    | PRecord Expr [((String,String),[ProcessCommand])]
        -- and records for some reason? but not in grammar...
  deriving (Eq,Show,Read,Generic,Out)

{-
----------------------------------------------------------------------------------------------------
--------------------------Concurrent DataTypes------------------------------------------------------
data ProcessCommand       =  PRun   (RowColPos,Ident,[Term ],[Channel],[Channel])
                           | PClose (RowColPos,Channel)
                           | PHalt  (RowColPos,[Channel])
                           | PGet   (RowColPos,Ident,Channel)
                           | PHcase (RowColPos,Channel,[ProcessPhrase_hcase ])
                           | PPut   (RowColPos, Term ,Channel)
                           | PHput  (RowColPos,Event_Handle,Channel)
                           | PSplit (RowColPos, Channel,[Channel])
                           | PPlug  (RowColPos,[Channel],([Channel],Process),([Channel],Process))
                           | PFork  (RowColPos,Channel,[ForkPart ])

                           | PCase  (RowColPoRowColPos,Term ,[ProcessPhrase_pcase])
                           | PRec   (RowColPos,[(Struct_Handle,[ProcessCommand])])  
                           | PEqual (RowColPos,Channel,Channel) -- PEqual (PChannel,PChannel)
                           deriving  (Eq,Show,Read,Generic,Out) 

-- | AC_PLUGf [CHANNEL] (CHANNELS,COMS) (CHANNELS,COMS)
-- | PPlug [Process]
-- | PPlug ([Channel],PlugPart,PlugPart)



----------------------------------------------------------------------------------------------------
--------------------------Channels for ID Command------------------------------------------------------
data PChannel             =   BareChannel String
                            | NegChannel String
                            deriving (Eq,Show,Read,Generic,Out)


data FuncName             =   Custom  (String,LineColPos)
                            | Inbuilt (Func,LineColPos)
                            deriving  (Eq,Show,Read,Generic,Out) 
-}

$( concat <$> traverse makeBaseFunctor 
    [ ''Expr
    , ''ProcessCommand 
    ]
 )
