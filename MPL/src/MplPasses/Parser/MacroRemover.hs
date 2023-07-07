

module MplPasses.Parser.MacroRemover (removeMacros) where

import MplPasses.Parser.BnfcParse as B

-- A module which resolves the 'on channel do x,y,z' syntax.
-- This is done by turning every instance of this syntax into an instance of the standard syntax (macro expansion)

-- Additionally, it converts instances of user-defined infix operators into standard function calls,
-- and turns user-defined infix operator definitions into standard function definitions.
-- Finally, it turns sectioned function calls "(infixOperator)(a,b)" into standard function calls.


removeMacros :: B.MplProg -> B.MplProg
removeMacros (MPL_PROG ls) = MPL_PROG (remStmts ls)

remStmts :: [B.MplStmt] -> [B.MplStmt]
remStmts = map remStmt

remStmt :: B.MplStmt -> B.MplStmt
remStmt (MPL_DEFN_STMS_WHERE a b) = MPL_DEFN_STMS_WHERE (remDefs a) (remWs b)
remStmt (MPL_DEFN_STMS a) = MPL_DEFN_STMS (map remDef a)
remStmt (MPL_STMT a) = MPL_STMT (remDef a)

remWs :: [B.MplWhere] -> [B.MplWhere]
remWs = map remDefsW

remDefsW :: B.MplWhere -> B.MplWhere
remDefsW (MPL_WHERE a) = MPL_WHERE (remStmt a)

remDefs :: [B.MplDefn] -> [B.MplDefn]
remDefs = map remDef

remDef :: B.MplDefn -> B.MplDefn
remDef (MPL_PROCESS_DEFN a) = MPL_PROCESS_DEFN (remProc a)
remDef a = a -- anything except a process definition can be left as-is.

remProc :: B.ProcessDefn -> B.ProcessDefn
remProc (TYPED_PROCESS_DEFN a b c d ls) = TYPED_PROCESS_DEFN a b c d (remPhrases ls)
remProc (INTERNAL_TYPED_PROCESS_DEFN a b ls) = INTERNAL_TYPED_PROCESS_DEFN a b (remPhrases ls)
remProc (PROCESS_DEFN a ls) = PROCESS_DEFN a (remPhrases ls)

remPhrases :: [ProcessPhrase] -> [ProcessPhrase]
remPhrases [] = []
remPhrases (b:bs) = (remPhrase b) : (remPhrases bs)

remPhrase :: ProcessPhrase -> ProcessPhrase
remPhrase (PROCESS_PHRASE a b c block) = PROCESS_PHRASE a b c (remBlock block)

-- From this point forward, we are dealing with removing macros.
-- Basically, we map each command with 'remC' to change it as needed,
-- And we use 'onUnwrap' to turn an 'on' block into a series of statements.


remBlock :: ProcessCommandsBlock -> ProcessCommandsBlock
remBlock (PROCESS_COMMANDS_DO_BLOCK cs) =
    PROCESS_COMMANDS_DO_BLOCK (remCs cs)
remBlock (PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK (PROCESS_ON id ps)) =
    PROCESS_COMMANDS_DO_BLOCK (remCs (onUnwrap id ps)) -- special case: a single 'on' block becomes a 'do' block.
remBlock (PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK c) = -- single command, not an 'on' block.
    PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK (remC c)

-- Remove macros from a list of commands. Everything else is basically just tree traversal;
-- this is where the interesting stuff happens.
remCs :: [ProcessCommand] -> [ProcessCommand]
remCs [] = []
remCs ((PROCESS_ON id ps):rs) =
    remCs ((onUnwrap id ps) ++ rs) -- Remove macro
remCs (r:rs) =
    (remC r) : (remCs rs) -- handle this case then go to the next.


-- This function removes macros from a single command.
-- Note that 'PROCESS_ON' is not included here, as it returns a list,
-- and a list needs to be handled separately (since that changes the structure of the AST)
remC :: ProcessCommand -> ProcessCommand
remC (PROCESS_HCASE a b ps) =
    PROCESS_HCASE a b (map remHCP ps)
remC (PROCESS_FORK a b ps) =
    PROCESS_FORK a b (map remFP ps)
remC (PROCESS_RACE ps) =
    PROCESS_RACE (map remRP ps)
remC (PROCESS_PLUG ps) =
    PROCESS_PLUG (map remPP ps)
remC (PROCESS_CASE a b ps) =
    PROCESS_CASE a b (map remPCP ps)
remC (PROCESS_IF a b1 b2) =
    PROCESS_IF a (remBlock b1) (remBlock b2)
remC (PROCESS_SWITCH ps) =
    PROCESS_SWITCH (map remPSP ps)
remC a = a -- The default case. do nothing.



remHCP :: HCasePhrase -> HCasePhrase
remHCP (HCASE_PHRASE a block) =
    HCASE_PHRASE a (remBlock block)

remFP :: ForkPhrase -> ForkPhrase
remFP (FORK_PHRASE a block) =
    FORK_PHRASE a (remBlock block)
remFP (FORK_WITH_PHRASE a cs block) =
    FORK_WITH_PHRASE a cs (remBlock block)

remRP :: RacePhrase -> RacePhrase
remRP (RACE_PHRASE a block) =
    RACE_PHRASE a (remBlock block)

remPP :: PlugPhrase -> PlugPhrase
remPP (PLUG_PHRASE block) =
    PLUG_PHRASE (remBlock block)
remPP (PLUG_PHRASE_AS a b block) =
    PLUG_PHRASE_AS a b (remBlock block)

remPCP :: ProcessCasePhrase -> ProcessCasePhrase
remPCP (PROCESS_CASE_PHRASE a block) =
    PROCESS_CASE_PHRASE a (remBlock block)

remPSP :: ProcessSwitchPhrase -> ProcessSwitchPhrase
remPSP (PROCESS_SWITCH_PHRASE a block) =
    PROCESS_SWITCH_PHRASE a (remBlock block)

-- Unwraps an 'on _ do x,y,z' to a series of discrete commands.
-- Note: those commands may also need to be unwrapped, so be sure to call remCs on the result of onUnwrap!
onUnwrap :: PIdent -> [OnPhrase] -> [ProcessCommand]
onUnwrap channel [] = []
onUnwrap channel@(PIdent (_,chan)) ((ON_PUT p@(Put (coords,_)) expr):rs) =
    (PROCESS_PUT p expr (PIdent (coords,chan))) : (onUnwrap channel rs)
onUnwrap channel@(PIdent (_,chan)) ((ON_GET g@(Get (coords,_)) patt):rs) =
    (PROCESS_GET g patt (PIdent (coords,chan))) : (onUnwrap channel rs)
onUnwrap channel@(PIdent (_,chan)) ((ON_HPUT h@(HPut (coords,_)) hand):rs) =
    (PROCESS_HPUT h hand (PIdent (coords,chan))) : (onUnwrap channel rs)
onUnwrap channel@(PIdent (_,chan)) ((ON_HCASE h@(HCase (coords,_)) phrases):rs) =
    (PROCESS_HCASE h (PIdent (coords,chan)) phrases) : (onUnwrap channel rs)
onUnwrap channel@(PIdent (_,chan)) ((ON_FORK h@(Fork (coords,_)) phrases):rs) =
    (PROCESS_FORK h (PIdent (coords,chan)) phrases) : (onUnwrap channel rs)
onUnwrap channel@(PIdent (_,chan)) ((ON_SPLIT h@(Split (coords,_)) phrases):rs) =
    (PROCESS_SPLIT h (PIdent (coords,chan)) phrases) : (onUnwrap channel rs)
onUnwrap channel@(PIdent (_,chan)) ((ON_CLOSE h@(Close (coords,_))):rs) =
    (PROCESS_CLOSE h (PIdent (coords,chan))) : (onUnwrap channel rs)
onUnwrap channel@(PIdent (_,chan)) ((ON_HALT h@(Halt (coords,_))):rs) =
    (PROCESS_HALT h (PIdent (coords,chan))) : (onUnwrap channel rs)
