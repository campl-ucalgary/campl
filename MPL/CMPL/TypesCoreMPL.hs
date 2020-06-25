{-# LANGUAGE DeriveGeneric #-}
module CMPL.TypesCoreMPL where 
import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint
import AMPL.TypesAMPL



type Name                 = String  
type Argument             = (String,PosnPair)
type Channel              = NamePnPair


type Handler              = (Event_Handle,Process )
type Event_Handle         = (NamePnPair,NamePnPair)
type Struct_Handle        = (NamePnPair,NamePnPair,[NamePnPair])
type Struct_Name          = (NamePnPair,NamePnPair)
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
type ProtocolClause       = (NamePnPair,[NamePnPair]) -- protocol name & list of handles 
type Process              = [ProcessCommand ]
type ProcessPhrase_pcase  = (Struct_Handle ,Process)
type ProcessPhrase_hcase  = (Event_Handle,Process )
type PatternDef           = (Struct_Handle ,[Term] )
type DataClause           = (NamePnPair,[Constructor])
type Constructor          = (NamePnPair,(Int,PosnPair))
type Stack                = [String]
type Translation          = [(String,POLARITY,Int)]

----------------------------------------------------------------------------------------------------
--------------------------MPL Constructs------------------------------------------------------
data MPLProg              = MPLProg  Include_Defns Data_Defns  Codata_Defns 
                                     Protocol_Defns CoProtocol_Defns
                                     Function_Defns Process_Defns  MainRun_Defn
                            deriving (Eq,Show,Read,Generic)

instance () => Out (MPLProg)

data Defn                 =   Includes [String]    
                            | Data   (PosnPair, DataClause)
                            | Codata (PosnPair, DataClause)
                            | Protocol (PosnPair, ProtocolClause)
                            | CoProtocol (PosnPair, ProtocolClause)
                            | Function (PosnPair, FuncName,[Argument],Term) 
                            | Process (PosnPair,NamePnPair,[Argument],[Channel],[Channel],[ProcessCommand])
                            | MainRun (PosnPair,[Channel],[Channel],[ProcessCommand]) 
                            deriving (Eq,Show,Read,Generic)


instance () => Out (Defn)

----------------------------------------------------------------------------------------------------
--------------------------Sequential DataTypes------------------------------------------------------


data Term                =    TCall   (FuncName,[Term]) 
                            | TCons   (Struct_Name,[Term]) 
                            | TDest   (NamePnPair,Struct_Name,[Term])
                            | TCase   (Term ,[PatternDef],PosnPair) -- last posn , beginning of case keyword
                            | TVar    (String,PosnPair)  
                            | TConstS (String,PosnPair)
                            | TConstC (Char,PosnPair)
                            | TConstI (Int,PosnPair)   
                            | TRec    ([(Struct_Handle,Term)],PosnPair)
                            | TProd   [Term]
                            | TProdElem (Int,Term,PosnPair)
                            | TError String 
                            deriving  (Eq,Show,Read,Generic)

instance () => Out (Term)


data FuncName             =   Custom  (String,PosnPair)
                            | Inbuilt (Func,PosnPair)
                            deriving  (Eq,Show,Read,Generic)

instance () => Out (FuncName)

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
                             deriving  (Eq,Show,Read,Generic)

instance () => Out (Func)

----------------------------------------------------------------------------------------------------
--------------------------Concurrent DataTypes------------------------------------------------------

data ProcessCommand       =  PRun   (PosnPair,NamePnPair,[Term ],[Channel],[Channel])
                           | PClose (PosnPair,Channel)
                           | PHalt  (PosnPair,[Channel])
                           | PGet   (PosnPair,NamePnPair,Channel)
                           | PHcase (PosnPair,Channel,[ProcessPhrase_hcase ])
                           | PPut   (PosnPair, Term ,Channel)
                           | PHput  (PosnPair,Event_Handle,Channel)
                           | PSplit (PosnPair, Channel,[Channel])
                           | PPlug  (PosnPair,[Channel],([Channel],Process),([Channel],Process))
                           | PFork  (PosnPair,Channel,[ForkPart ])
                           | PCase  (PosnPair,Term ,[ProcessPhrase_pcase])
                           | PRec   (PosnPair,[(Struct_Handle,[ProcessCommand])])  
                           | PEqual (PosnPair,Channel,Channel) -- PEqual (PChannel,PChannel)
                           deriving  (Eq,Show,Read,Generic) 

-- | AC_PLUGf [CHANNEL] (CHANNELS,COMS) (CHANNELS,COMS)
-- | PPlug [Process]
-- | PPlug ([Channel],PlugPart,PlugPart)

instance () => Out (ProcessCommand)


----------------------------------------------------------------------------------------------------
--------------------------Channels for ID Command------------------------------------------------------
data PChannel             =   BareChannel String
                            | NegChannel String
                            deriving (Eq,Show,Read,Generic)

instance () => Out (PChannel)