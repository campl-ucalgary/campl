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
remDef (MPL_FUNCTION_DEFN a) = MPL_FUNCTION_DEFN (remFunc a)
remDef a = a -- anything except a process definition can be left as-is.

remProc :: B.ProcessDefn -> B.ProcessDefn
remProc (TYPED_PROCESS_DEFN a (PROCESS_TYPE b c d) ls) = TYPED_PROCESS_DEFN a (PROCESS_TYPE b c d) (remPhrases ls)
remProc (INTERNAL_TYPED_PROCESS_DEFN a b ls) = INTERNAL_TYPED_PROCESS_DEFN a b (remPhrases ls)
remProc (PROCESS_DEFN a ls) = PROCESS_DEFN a (remPhrases ls)

remPhrases :: [ProcessPhrase] -> [ProcessPhrase]
remPhrases [] = []
remPhrases (b : bs) = (remPhrase b) : (remPhrases bs)

remPhrase :: ProcessPhrase -> ProcessPhrase
remPhrase (PROCESS_PHRASE a b c block) = PROCESS_PHRASE a b c (remBlock block)

---------------------------------------------------------------------------
-- Unwrapping the 'on' syntax
---------------------------------------------------------------------------

-- From this point forward, we are dealing with removing macros.
-- Basically, we map each command with 'remC' to change it as needed,
-- And we use 'onUnwrap' to turn an 'on' block into a series of statements.

remBlock :: ProcessCommandsBlock -> ProcessCommandsBlock
remBlock (PROCESS_COMMANDS_DO_BLOCK cs) =
  PROCESS_COMMANDS_DO_BLOCK (remCs cs)
remBlock (PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK (PROCESS_ON id ps)) =
  PROCESS_COMMANDS_DO_BLOCK (remCs (onUnwrap id ps)) -- special case: a single 'on' block becomes a 'do' block.
remBlock (PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK c) =
  -- single command, not an 'on' block.
  PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK (remC c)

-- Remove macros from a list of commands. Everything else is basically just tree traversal;
-- this is where the interesting stuff happens.
remCs :: [ProcessCommand] -> [ProcessCommand]
remCs [] = []
remCs ((PROCESS_ON id ps) : rs) =
  remCs ((onUnwrap id ps) ++ rs) -- Remove macro
remCs (r : rs) =
  (remC r) : (remCs rs) -- handle this case then go to the next.

-- This function removes macros from a single command.
-- Note that 'PROCESS_ON' is not included here, as it returns a list,
-- and a list needs to be handled separately (since that changes the structure of the AST)
remC :: ProcessCommand -> ProcessCommand
-- Commands which can contain 'on' blocks inside
remC (PROCESS_HCASE a b ps) =
  PROCESS_HCASE a b (map remHCP ps)
remC (PROCESS_FORK a b ps) =
  PROCESS_FORK a b (map remFP ps)
remC (PROCESS_RACE ps) =
  PROCESS_RACE (map remRP ps)
remC (PROCESS_PLUG ps) =
  PROCESS_PLUG (map remPP ps)
-- commands which can contain expressions inside.
remC (PROCESS_RUN i lb es i1s i2s rb) =
  PROCESS_RUN i lb (map remExp es) i1s i2s rb
remC (PROCESS_PUT put e i) =
  PROCESS_PUT put (remExp e) i
-- commands which contain both command blocks and expressions
remC (PROCESS_CASE cas e ps) =
  PROCESS_CASE cas (remExp e) (map remPCP ps)
remC (PROCESS_IF e b1 b2) =
  PROCESS_IF (remExp e) (remBlock b1) (remBlock b2)
remC (PROCESS_SWITCH ps) =
  PROCESS_SWITCH (map remPSP ps)
-- Removing qualified function calls
remC (PROCESS_QRUN modName objName lb es i1s i2s rb) =
  remC $
    PROCESS_RUN
      (PROCESS_NAME (mergeQualifiedName modName objName))
      lb
      es
      i1s
      i2s
      rb
-- The default case. do nothing.
remC a = a

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

-- Unwraps an 'on _ do x,y,z' to a series of discrete commands.
-- Note: those commands may also need to be unwrapped, so be sure to call remCs on the result of onUnwrap!
onUnwrap :: PIdent -> [OnPhrase] -> [ProcessCommand]
onUnwrap channel [] = []
onUnwrap channel@(PIdent (_, chan)) ((ON_PUT p@(Put (coords, _)) expr) : rs) =
  (PROCESS_PUT p expr (PIdent (coords, chan))) : (onUnwrap channel rs)
onUnwrap channel@(PIdent (_, chan)) ((ON_GET g@(Get (coords, _)) patt) : rs) =
  (PROCESS_GET g patt (PIdent (coords, chan))) : (onUnwrap channel rs)
onUnwrap channel@(PIdent (_, chan)) ((ON_HPUT h@(HPut (coords, _)) hand) : rs) =
  (PROCESS_HPUT h hand (PIdent (coords, chan))) : (onUnwrap channel rs)
onUnwrap channel@(PIdent (_, chan)) ((ON_HCASE h@(HCase (coords, _)) phrases) : rs) =
  (PROCESS_HCASE h (PIdent (coords, chan)) phrases) : (onUnwrap channel rs)
onUnwrap channel@(PIdent (_, chan)) ((ON_FORK h@(Fork (coords, _)) phrases) : rs) =
  (PROCESS_FORK h (PIdent (coords, chan)) phrases) : (onUnwrap channel rs)
onUnwrap channel@(PIdent (_, chan)) ((ON_SPLIT h@(Split (coords, _)) phrases) : rs) =
  (PROCESS_SPLIT h (PIdent (coords, chan)) phrases) : (onUnwrap channel rs)
onUnwrap channel@(PIdent (_, chan)) ((ON_CLOSE h@(Close (coords, _))) : rs) =
  (PROCESS_CLOSE h (PIdent (coords, chan))) : (onUnwrap channel rs)
onUnwrap channel@(PIdent (_, chan)) ((ON_HALT h@(Halt (coords, _))) : rs) =
  (PROCESS_HALT h (PIdent (coords, chan))) : (onUnwrap channel rs)

---------------------------------------------------------------------------
-- Resolving instances of infix operators and declarations
---------------------------------------------------------------------------

-- Several expression phrases.

remLEP :: LetExprPhrase -> LetExprPhrase
remLEP (LET_EXPR_PHRASE m) = LET_EXPR_PHRASE (remStmt m)

remFEP :: FoldExprPhrase -> FoldExprPhrase
remFEP (FOLD_EXPR_PHRASE i colon ps e) =
  FOLD_EXPR_PHRASE i colon ps (remExp e)

remPEP :: PattExprPhrase -> PattExprPhrase
remPEP (PATTERN_TO_EXPR ps e) =
  PATTERN_TO_EXPR ps (remExp e)

remUEP :: UnfoldExprPhrase -> UnfoldExprPhrase
remUEP (UNFOLD_EXPR_PHRASE p feps) =
  UNFOLD_EXPR_PHRASE p (map remFEP feps)

remSEP :: SwitchExprPhrase -> SwitchExprPhrase
remSEP (SWITCH_EXPR_PHRASE e1 e2) =
  SWITCH_EXPR_PHRASE (remExp e1) (remExp e2)

remREP :: RecordExprPhrase -> RecordExprPhrase
remREP (RECORD_EXPR_PHRASE i e) =
  RECORD_EXPR_PHRASE i (remExp e)
remREP (RECORD_EXPR_HIGHER_ORDER_PHRASE i pep) =
  RECORD_EXPR_HIGHER_ORDER_PHRASE i (remPEP pep)

remTEL :: TupleExprList -> TupleExprList
remTEL (TUPLE_EXPR_LIST e) =
  TUPLE_EXPR_LIST (remExp e)

-- This one is both for fixing 'on' blocks and expressions.
remPSP :: ProcessSwitchPhrase -> ProcessSwitchPhrase
remPSP (PROCESS_SWITCH_PHRASE e block) =
  PROCESS_SWITCH_PHRASE (remExp e) (remBlock block)

-- Fixes up functions: converts prefix definitions to standard definitions,
remFunc :: FunctionDefn -> FunctionDefn
remFunc (TYPED_FUNCTION_DEFN_UINFIX _ i _ t1 t2 t3 ps) =
  remFunc $ TYPED_FUNCTION_DEFN (convertToPIdent i) [t1, t2] t3 ps
remFunc (FUNCTION_DEFN_UINFIX _ i _ ps) =
  remFunc $ FUNCTION_DEFN (convertToPIdent i) ps
remFunc (TYPED_FUNCTION_DEFN i t1s t2 ps) =
  TYPED_FUNCTION_DEFN i t1s t2 (map remPEP ps)
remFunc (FUNCTION_DEFN i ps) =
  FUNCTION_DEFN i (map remPEP ps)

-- Converts a user-defined infix operator to a generic function identifier.
-- This allows infix function declarations/usages to become prefix instead.
-- Basically just unwrapping and rewrapping.
-- Note that 'a' is just an ((Int,Int),String), which is a coordinate followed by a string token.
convertToPIdent :: InfixUop -> PIdent
convertToPIdent (InfixUop1 (InfixU1op a)) = PIdent a
convertToPIdent (InfixUop2 (InfixU2op a)) = PIdent a
convertToPIdent (InfixUop3 (InfixU3op a)) = PIdent a
convertToPIdent (InfixUop5 (InfixU5op a)) = PIdent a
convertToPIdent (InfixUop6 (InfixU6op a)) = PIdent a
convertToPIdent (InfixUop7 (InfixU7op a)) = PIdent a

-- Traverses an expression, taking any instance of a user-defined infix operator
-- and making it a function call. Because the user-defined infix definition is also converted to a function definition,
-- the system behaves like infix operators are infix, and functions are functions,
-- but secretly everything is just a function.
-- IMPORTANT: make sure the 'remExp' function is being called on every 'Expr' node in the AST, because otherwise,
--  the infix operators won't all be replaced, and you will get an error later on because an infix AST node wasn't replaced.
remExp :: Expr -> Expr
remExp (EXPR e) = EXPR (remExp e)
remExp (TYPED_EXPR e tp) = TYPED_EXPR (remExp e) tp
remExp (IF_EXPR e1 e2 e3) = IF_EXPR (remExp e1) (remExp e2) (remExp e3)
remExp (LET_EXPR leps e) = LET_EXPR (map remLEP leps) (remExp e)
remExp (INFIXR0_EXPR e1 colon e2) = INFIXR0_EXPR (remExp e1) colon (remExp e2)
remExp (INFIXL1_EXPR e1 iop e2) = INFIXL1_EXPR (remExp e1) iop (remExp e2)
remExp (INFIXL2_EXPR e1 iop e2) = INFIXL2_EXPR (remExp e1) iop (remExp e2)
remExp (INFIXL3_EXPR e1 iop e2) = INFIXL3_EXPR (remExp e1) iop (remExp e2)
remExp (INFIXL4_EXPR e1 iop e2) = INFIXL4_EXPR (remExp e1) iop (remExp e2)
remExp (INFIXL5_EXPR e1 iop e2) = INFIXL5_EXPR (remExp e1) iop (remExp e2)
remExp (INFIXL6_EXPR e1 iop e2) = INFIXL6_EXPR (remExp e1) iop (remExp e2)
remExp (INFIXR7_EXPR e1 iop e2) = INFIXR7_EXPR (remExp e1) iop (remExp e2)
remExp (INFIXL8_EXPR e1 iop e2) = INFIXL8_EXPR (remExp e1) iop (remExp e2)
remExp (LIST_EXPR lb es rb) = LIST_EXPR lb (map remExp es) rb
remExp (VAR_EXPR i) = VAR_EXPR i
remExp (INT_EXPR i) = INT_EXPR i
remExp (STRING_EXPR s) = STRING_EXPR s
remExp (CHAR_EXPR c) = CHAR_EXPR c
remExp (DOUBLE_EXPR d) = DOUBLE_EXPR d
remExp (UNIT_EXPR lb rb) = UNIT_EXPR lb rb
remExp (FOLD_EXPR e feps) = FOLD_EXPR (remExp e) (map remFEP feps)
remExp (UNFOLD_EXPR e ueps) = UNFOLD_EXPR (remExp e) (map remUEP ueps)
remExp (CASE_EXPR c e peps) = CASE_EXPR c (remExp e) (map remPEP peps)
remExp (SWITCH_EXP seps) = SWITCH_EXP (map remSEP seps)
remExp (STORE_EXPR s l p r) = STORE_EXPR s l p' r
  where
    p' = case p of
      PROCESS_P phr -> PROCESS_P $ remPhrase phr
      _ -> p
remExp (DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR i lb es rb) =
  DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR i lb (map remExp es) rb
remExp (DESTRUCTOR_CONSTRUCTOR_NO_ARGS_EXPR i) =
  DESTRUCTOR_CONSTRUCTOR_NO_ARGS_EXPR i
remExp (TUPLE_EXPR lb e tels rb) =
  TUPLE_EXPR lb (remExp e) (map remTEL tels) rb
remExp (FUN_EXPR i lb es rb) =
  FUN_EXPR i lb (map remExp es) rb
remExp (RECORD_EXPR lb reps rb) =
  RECORD_EXPR lb (map remREP reps) rb
remExp (BRACKETED_EXPR lb e rb) =
  BRACKETED_EXPR lb (remExp e) rb
-- A qualified function call: merge the module name and function name.
remExp (FUNQ_EXPR modName funcName lb es rb) =
  remExp $
    FUN_EXPR
      (mergeQualifiedName modName funcName)
      lb
      es
      rb
-- A sectioned infix operator: literally just ignore the extra brackets,
-- and turn the InfixUop into a standard identifier.
remExp (INFIXU_SECT _ i _ lb e1 e2 rb) =
  remExp $
    FUN_EXPR
      (convertToPIdent i)
      lb
      [e1, e2]
      rb
-- A sectioned built-in infix operator: convert from prefix to infix, throw away bracket tokens.
-- Afterwards, recurse into each sub-expression
remExp (INFIXL1_SECT _ iop _ _ e1 e2 _) = remExp $ INFIXL1_EXPR e1 iop e2
remExp (INFIXL2_SECT _ iop _ _ e1 e2 _) = remExp $ INFIXL2_EXPR e1 iop e2
remExp (INFIXL3_SECT _ iop _ _ e1 e2 _) = remExp $ INFIXL3_EXPR e1 iop e2
remExp (INFIXL4_SECT _ iop _ _ e1 e2 _) = remExp $ INFIXL4_EXPR e1 iop e2
remExp (INFIXL5_SECT _ iop _ _ e1 e2 _) = remExp $ INFIXL5_EXPR e1 iop e2
remExp (INFIXL6_SECT _ iop _ _ e1 e2 _) = remExp $ INFIXL6_EXPR e1 iop e2
remExp (INFIXR7_SECT _ iop _ _ e1 e2 _) = remExp $ INFIXR7_EXPR e1 iop e2
remExp (INFIXL8_SECT _ iop _ _ e1 e2 _) = remExp $ INFIXL8_EXPR e1 iop e2
-- A sectioned '+' or '*' is harder, because they are interpreted as tensor and par.
-- As a result, we need to change the 'x' coordinate on each token to make things work.
remExp (INFIXPR_SECT (Par ((y, x), _)) _ e1 e2 _) =
  remExp $ INFIXL5_EXPR e1 (Infixl5op ((y, x + 1), "+")) e2
remExp (INFIXTN_SECT (Tensor ((y, x), _)) _ e1 e2 _) =
  remExp $ INFIXL6_EXPR e1 (Infixl6op ((y, x + 1), "*")) e2
-- Below are all of the infix operator replacements. They are all effectively the same,
-- except for the constructor names in the first line of each definition.
-- replace infix operator precedence 1
remExp (INFIXU1_EXPR e1 (InfixU1op (coords, str)) e2) =
  remExp $
    FUN_EXPR
      (PIdent (coords, str))
      (LBracket (coords, "("))
      [e1, e2] -- these expressions get resolved by the fact that we apply 'remExp' on the result of this.
      (RBracket (coords, ")"))
-- replace infix operator precedence 2
remExp (INFIXU2_EXPR e1 (InfixU2op (coords, str)) e2) =
  remExp $
    FUN_EXPR
      (PIdent (coords, str))
      (LBracket (coords, "("))
      [e1, e2] -- these expressions get resolved by the fact that we apply 'remExp' on the result of this.
      (RBracket (coords, ")"))
-- replace infix operator precedence 3
remExp (INFIXU3_EXPR e1 (InfixU3op (coords, str)) e2) =
  remExp $
    FUN_EXPR
      (PIdent (coords, str))
      (LBracket (coords, "("))
      [e1, e2] -- these expressions get resolved by the fact that we apply 'remExp' on the result of this.
      (RBracket (coords, ")"))
-- replace infix operator precedence 5
remExp (INFIXU5_EXPR e1 (InfixU5op (coords, str)) e2) =
  remExp $
    FUN_EXPR
      (PIdent (coords, str))
      (LBracket (coords, "("))
      [e1, e2] -- these expressions get resolved by the fact that we apply 'remExp' on the result of this.
      (RBracket (coords, ")"))
-- replace infix operator precedence 6
remExp (INFIXU6_EXPR e1 (InfixU6op (coords, str)) e2) =
  remExp $
    FUN_EXPR
      (PIdent (coords, str))
      (LBracket (coords, "("))
      [e1, e2] -- these expressions get resolved by the fact that we apply 'remExp' on the result of this.
      (RBracket (coords, ")"))
-- replace infix operator precedence 7
remExp (INFIXU7_EXPR e1 (InfixU7op (coords, str)) e2) =
  remExp $
    FUN_EXPR
      (PIdent (coords, str))
      (LBracket (coords, "("))
      [e1, e2] -- these expressions get resolved by the fact that we apply 'remExp' on the result of this.
      (RBracket (coords, ")"))

---------------------------------------------------------------------------
-- Resolving qualified functions/processes
---------------------------------------------------------------------------

-- turns a:
--   "Module" "." "obj"
-- into:
--   "Module.obj"
-- that way, we can get rid of the new AST nodes for the qualified references.
mergeQualifiedName :: UIdent -> PIdent -> PIdent
mergeQualifiedName (UIdent (pos, modName)) (PIdent (_, objName)) =
  PIdent (pos, modName ++ ('.' : objName))