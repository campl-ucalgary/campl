{- | Resolves the built-in operators @||@, @&&@, and @++@.

The lexer's user-defined-operator rules shadow the grammar's
dedicated operator tokens ('InfixU1op' beats 'Infixl1op' for @||@,
'InfixU2op' beats 'Infixl2op' for @&&@, and 'InfixU5op' beats
'Infixl4op' for @++@; see @MPL.bnfc@), so by the time
'MplPasses.Parser.MacroRemover.removeMacros' has run, every use of
one of these operators is an ordinary call to a function literally
named "||", "&&", or "++" --- whether or not any such function is
defined. This pass gives those calls a built-in meaning, in one of
two ways chosen per operator:

  * @||@ and @&&@ are rewritten to @if@-expressions, which makes
    them short-circuit (the machine only evaluates the branch it
    takes):

    >  p || q   ~>   if p then True else q
    >  p && q   ~>   if p then q else False

  * @++@ cannot be expressed as a local rewrite (append is
    recursive), so instead a definition of @(++)@ is appended to the
    program when it is used but never defined:

    >  fun (++) =
    >      x:xs, ys -> x : (xs ++ ys)
    >      [],   ys -> ys

    The definition is prepended as the first statement of the
    program: the renamer resolves each statement against the
    statements before it, so a trailing definition would not be
    visible to earlier uses.

In both cases a user definition of the operator anywhere in the
program (top level, a @where@ block, or a @let@) takes precedence:
the pass leaves that operator's uses alone, and they keep calling
the user's definition at whatever type the user gave it.

Two design constraints worth knowing about:

  * This must run before the renamer: the renamer's symbol table is
    tied lazily with recursive-do, so "is this name in scope?"
    cannot be asked during renaming without forcing the knot
    (@<<loop>>@).

  * The defined-operator check is whole-program rather than
    per-scope, again because scope information does not exist yet at
    this stage. A user definition of an operator therefore disables
    the built-in everywhere, which conservatively preserves the
    behavior of every program that compiled before this pass
    existed.
-}
{-# LANGUAGE FlexibleContexts #-}
module MplPasses.Parser.ResolveBuiltinOps (resolveBuiltinOps) where

import MplPasses.Parser.BnfcParse as B

import Control.Monad.Writer
import qualified Data.Set as Set

-- | The names this pass gives a built-in meaning when the program
-- does not define them.
builtinOpNames :: Set.Set String
builtinOpNames = Set.fromList ["||", "&&", "++"]

-- | What the walk collects: names of the operators the program
-- defines itself, and names of the operators it uses.
type OpUsage = (Set.Set String, Set.Set String)

resolveBuiltinOps :: B.MplProg -> B.MplProg
resolveBuiltinOps prog = MPL_PROG (appendDefn ++ stmts')
  where
    -- First walk: collect user definitions and uses of the built-in
    -- operator names (the expression hook is the identity).
    (userDefined, used) = execWriter $ walk id prog

    -- Second walk: rewrite the boolean operators the user did NOT
    -- define. The same traversal is reused; the collected names are
    -- ignored this time.
    (MPL_PROG stmts', _) = runWriter $ walk (rewriteBoolOp userDefined) prog

    -- @++@ is handled by injecting a definition rather than by
    -- rewriting each use; the uses are already in the right shape
    -- (calls to a function named "++").
    appendDefn
        | "++" `Set.member` used
        , not ("++" `Set.member` userDefined) = [builtinAppendDefn]
        | otherwise = []

-- | Rewrites one (already recursively-walked) expression. After
-- 'removeMacros', a use of a built-in operator is exactly a
-- 'FUN_EXPR' whose name is the operator and whose argument list has
-- two entries; an identifier lexed as 'PIdent' can never contain
-- @|@, @&@, or @+@, so nothing else can collide with these names.
rewriteBoolOp :: Set.Set String -> Expr -> Expr
rewriteBoolOp userDefined expr = case expr of
    FUN_EXPR (PIdent (pos, op)) _ [l, r] _
        | builtin op, op == "||" -> IF_EXPR l (boolExpr pos "True") r
        | builtin op, op == "&&" -> IF_EXPR l r (boolExpr pos "False")
    _ -> expr
  where
    builtin op = op `Set.member` builtinOpNames
              && not (op `Set.member` userDefined)
    -- True/False parse to the built-in Bool constructors (see
    -- 'exprDestructorConstructorParse' in Parse.hs).
    boolExpr pos b = DESTRUCTOR_CONSTRUCTOR_NO_ARGS_EXPR (UIdent (pos, b))

{- | The injected list append, equivalent to the source text

>  fun (++) =
>      x:xs, ys -> x : (xs ++ ys)
>      [],   ys -> ys

The phrase order (cons case first) mirrors the hand-written appends
in the test corpus, which the exhaustiveness checker accepts. All
tokens carry the invalid position (-1,-1): the definition has no
source location, and any type error at a use site is reported
against the use's own position.
-}
builtinAppendDefn :: B.MplStmt
builtinAppendDefn =
    MPL_STMT $ MPL_FUNCTION_DEFN $ FUNCTION_DEFN (pident "++")
        [ PATTERN_TO_EXPR
            [ LIST_COLON_PATTERN
                (VAR_PATTERN (pident "x")) colon
                (VAR_PATTERN (pident "xs"))
            , VAR_PATTERN (pident "ys")
            ]
            (INFIXR0_EXPR (var "x") colon
                (FUN_EXPR (pident "++") lbr [var "xs", var "ys"] rbr))
        , PATTERN_TO_EXPR
            [ LIST_PATTERN
                (LSquareBracket (npos, "[")) [] (RSquareBracket (npos, "]"))
            , VAR_PATTERN (pident "ys")
            ]
            (var "ys")
        ]
  where
    npos = (-1, -1)
    pident s = PIdent (npos, s)
    var = VAR_EXPR . pident
    colon = Colon (npos, ":")
    lbr = LBracket (npos, "(")
    rbr = RBracket (npos, ")")

{- | Walks the whole program: applies the hook to every expression
(bottom-up, so the hook sees children already rewritten), tells the
name of every function definition that could shadow a built-in
operator, and tells every use of a built-in operator name. Runs
after 'removeMacros', so infix definitions have already been
converted to prefix 'FUNCTION_DEFN' forms and the
@_UINFIX@/'PROCESS_ON'/qualified-name constructors cannot occur ---
they are still traversed, for totality's sake.

Every constructor that (transitively) contains an 'Expr' must be
covered here, mirroring the constraint documented on 'remExp' in
MacroRemover. A missed expression position fails safe: a built-in
operator in it would stay a call to an undefined function and be
reported as out of scope by the renamer, rather than silently
changing meaning.
-}
walk :: (Expr -> Expr) -> B.MplProg -> Writer OpUsage B.MplProg
walk hook (MPL_PROG stmts) = MPL_PROG <$> traverse wStmt stmts
  where
    wStmt (MPL_DEFN_STMS_WHERE ds ws) =
        MPL_DEFN_STMS_WHERE <$> traverse wDefn ds <*> traverse wWhere ws
    wStmt (MPL_DEFN_STMS ds) = MPL_DEFN_STMS <$> traverse wDefn ds
    wStmt (MPL_STMT d) = MPL_STMT <$> wDefn d

    wWhere (MPL_WHERE s) = MPL_WHERE <$> wStmt s

    wDefn (MPL_FUNCTION_DEFN f) = MPL_FUNCTION_DEFN <$> wFunc f
    wDefn (MPL_PROCESS_DEFN p) = MPL_PROCESS_DEFN <$> wProc p
    -- type / import definitions contain no expressions.
    wDefn d = pure d

    wFunc (TYPED_FUNCTION_DEFN i ts t ps) =
        noteDefn i >> TYPED_FUNCTION_DEFN i ts t <$> traverse wPEP ps
    wFunc (FUNCTION_DEFN i ps) =
        noteDefn i >> FUNCTION_DEFN i <$> traverse wPEP ps
    wFunc (INTERNAL_TYPED_FUNCTION_DEFN i t ps) =
        noteDefn i >> INTERNAL_TYPED_FUNCTION_DEFN i t <$> traverse wPEP ps
    -- gone after removeMacros, but walked for totality:
    wFunc (TYPED_FUNCTION_DEFN_UINFIX l op r t0 t1 t2 ps) =
        TYPED_FUNCTION_DEFN_UINFIX l op r t0 t1 t2 <$> traverse wPEP ps
    wFunc (FUNCTION_DEFN_UINFIX l op r ps) =
        FUNCTION_DEFN_UINFIX l op r <$> traverse wPEP ps

    -- record every function definition named like a built-in
    -- operator; this is what disables that operator's built-in
    -- meaning.
    noteDefn (PIdent (_, nm)) =
        when (nm `Set.member` builtinOpNames) $
            tell (Set.singleton nm, mempty)

    -- record every use of a built-in operator name; this is what
    -- triggers the injection of the @(++)@ definition.
    noteUse (PIdent (_, nm)) =
        when (nm `Set.member` builtinOpNames) $
            tell (mempty, Set.singleton nm)

    wProc (TYPED_PROCESS_DEFN i t ps) =
        TYPED_PROCESS_DEFN i t <$> traverse wPhrase ps
    wProc (INTERNAL_TYPED_PROCESS_DEFN i t ps) =
        INTERNAL_TYPED_PROCESS_DEFN i t <$> traverse wPhrase ps
    wProc (PROCESS_DEFN i ps) = PROCESS_DEFN i <$> traverse wPhrase ps

    wPhrase (PROCESS_PHRASE patts ins outs blk) =
        PROCESS_PHRASE patts ins outs <$> wBlock blk

    wBlock (PROCESS_COMMANDS_DO_BLOCK cs) =
        PROCESS_COMMANDS_DO_BLOCK <$> traverse wCmd cs
    wBlock (PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK c) =
        PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK <$> wCmd c

    wCmd (PROCESS_RUN pc lb es i1s i2s rb) =
        PROCESS_RUN <$> wPCall pc <*> pure lb <*> traverse wExpr es
                    <*> pure i1s <*> pure i2s <*> pure rb
    wCmd (PROCESS_QRUN m i lb es i1s i2s rb) =
        PROCESS_QRUN m i lb <$> traverse wExpr es
                            <*> pure i1s <*> pure i2s <*> pure rb
    wCmd (PROCESS_PUT put e i) = PROCESS_PUT put <$> wExpr e <*> pure i
    wCmd (PROCESS_HCASE h i ps) = PROCESS_HCASE h i <$> traverse wHCP ps
    wCmd (PROCESS_ON i ps) = PROCESS_ON i <$> traverse wOnP ps
    wCmd (PROCESS_FORK f i ps) = PROCESS_FORK f i <$> traverse wFP ps
    wCmd (PROCESS_RACE ps) = PROCESS_RACE <$> traverse wRP ps
    wCmd (PROCESS_PLUG ps) = PROCESS_PLUG <$> traverse wPP ps
    wCmd (PROCESS_CASE c e ps) =
        PROCESS_CASE c <$> wExpr e <*> traverse wPCP ps
    wCmd (PROCESS_IF e b1 b2) =
        PROCESS_IF <$> wExpr e <*> wBlock b1 <*> wBlock b2
    wCmd (PROCESS_SWITCH ps) = PROCESS_SWITCH <$> traverse wPSP ps
    -- close / halt / get / hput / split / id / neg: no expressions.
    wCmd c = pure c

    wPCall (PROCESS_USE u lb e rb) = PROCESS_USE u lb <$> wExpr e <*> pure rb
    wPCall pc = pure pc

    wHCP (HCASE_PHRASE i blk) = HCASE_PHRASE i <$> wBlock blk

    wOnP (ON_PUT p e) = ON_PUT p <$> wExpr e
    wOnP (ON_HCASE h ps) = ON_HCASE h <$> traverse wHCP ps
    wOnP (ON_FORK f ps) = ON_FORK f <$> traverse wFP ps
    wOnP p = pure p

    wFP (FORK_PHRASE i blk) = FORK_PHRASE i <$> wBlock blk
    wFP (FORK_WITH_PHRASE i cs blk) = FORK_WITH_PHRASE i cs <$> wBlock blk

    wRP (RACE_PHRASE i blk) = RACE_PHRASE i <$> wBlock blk

    wPP (PLUG_PHRASE blk) = PLUG_PHRASE <$> wBlock blk
    wPP (PLUG_PHRASE_AS a b blk) = PLUG_PHRASE_AS a b <$> wBlock blk

    wPCP (PROCESS_CASE_PHRASE patt blk) =
        PROCESS_CASE_PHRASE patt <$> wBlock blk

    wPSP (PROCESS_SWITCH_PHRASE e blk) =
        PROCESS_SWITCH_PHRASE <$> wExpr e <*> wBlock blk

    wLEP (LET_EXPR_PHRASE s) = LET_EXPR_PHRASE <$> wStmt s

    wFEP (FOLD_EXPR_PHRASE i c ps e) = FOLD_EXPR_PHRASE i c ps <$> wExpr e

    wUEP (UNFOLD_EXPR_PHRASE p feps) =
        UNFOLD_EXPR_PHRASE p <$> traverse wFEP feps

    wSEP (SWITCH_EXPR_PHRASE e1 e2) =
        SWITCH_EXPR_PHRASE <$> wExpr e1 <*> wExpr e2

    wREP (RECORD_EXPR_PHRASE i e) = RECORD_EXPR_PHRASE i <$> wExpr e
    wREP (RECORD_EXPR_HIGHER_ORDER_PHRASE i pep) =
        RECORD_EXPR_HIGHER_ORDER_PHRASE i <$> wPEP pep

    wPEP (PATTERN_TO_EXPR ps e) = PATTERN_TO_EXPR ps <$> wExpr e

    wTEL (TUPLE_EXPR_LIST e) = TUPLE_EXPR_LIST <$> wExpr e

    wNp (PROCESS_P phr) = PROCESS_P <$> wPhrase phr
    wNp np = pure np

    -- children first, then the hook, so the hook sees a node whose
    -- subexpressions are already resolved.
    wExpr e0 = hook <$> case e0 of
        EXPR e -> EXPR <$> wExpr e
        TYPED_EXPR e t -> TYPED_EXPR <$> wExpr e <*> pure t
        IF_EXPR e1 e2 e3 -> IF_EXPR <$> wExpr e1 <*> wExpr e2 <*> wExpr e3
        LET_EXPR ps e -> LET_EXPR <$> traverse wLEP ps <*> wExpr e
        INFIXR0_EXPR a op b -> INFIXR0_EXPR <$> wExpr a <*> pure op <*> wExpr b
        INFIXL1_EXPR a op b -> INFIXL1_EXPR <$> wExpr a <*> pure op <*> wExpr b
        INFIXU1_EXPR a op b -> INFIXU1_EXPR <$> wExpr a <*> pure op <*> wExpr b
        INFIXL2_EXPR a op b -> INFIXL2_EXPR <$> wExpr a <*> pure op <*> wExpr b
        INFIXU2_EXPR a op b -> INFIXU2_EXPR <$> wExpr a <*> pure op <*> wExpr b
        INFIXL3_EXPR a op b -> INFIXL3_EXPR <$> wExpr a <*> pure op <*> wExpr b
        INFIXU3_EXPR a op b -> INFIXU3_EXPR <$> wExpr a <*> pure op <*> wExpr b
        INFIXL4_EXPR a op b -> INFIXL4_EXPR <$> wExpr a <*> pure op <*> wExpr b
        INFIXL5_EXPR a op b -> INFIXL5_EXPR <$> wExpr a <*> pure op <*> wExpr b
        INFIXU5_EXPR a op b -> INFIXU5_EXPR <$> wExpr a <*> pure op <*> wExpr b
        INFIXL6_EXPR a op b -> INFIXL6_EXPR <$> wExpr a <*> pure op <*> wExpr b
        INFIXU6_EXPR a op b -> INFIXU6_EXPR <$> wExpr a <*> pure op <*> wExpr b
        INFIXR7_EXPR a op b -> INFIXR7_EXPR <$> wExpr a <*> pure op <*> wExpr b
        INFIXU7_EXPR a op b -> INFIXU7_EXPR <$> wExpr a <*> pure op <*> wExpr b
        INFIXL8_EXPR a op b -> INFIXL8_EXPR <$> wExpr a <*> pure op <*> wExpr b
        INFIXU_SECT l0 op r0 lb e1 e2 rb ->
            INFIXU_SECT l0 op r0 lb <$> wExpr e1 <*> wExpr e2 <*> pure rb
        INFIXL1_SECT l0 op r0 lb e1 e2 rb ->
            INFIXL1_SECT l0 op r0 lb <$> wExpr e1 <*> wExpr e2 <*> pure rb
        INFIXL2_SECT l0 op r0 lb e1 e2 rb ->
            INFIXL2_SECT l0 op r0 lb <$> wExpr e1 <*> wExpr e2 <*> pure rb
        INFIXL3_SECT l0 op r0 lb e1 e2 rb ->
            INFIXL3_SECT l0 op r0 lb <$> wExpr e1 <*> wExpr e2 <*> pure rb
        INFIXL4_SECT l0 op r0 lb e1 e2 rb ->
            INFIXL4_SECT l0 op r0 lb <$> wExpr e1 <*> wExpr e2 <*> pure rb
        INFIXL5_SECT l0 op r0 lb e1 e2 rb ->
            INFIXL5_SECT l0 op r0 lb <$> wExpr e1 <*> wExpr e2 <*> pure rb
        INFIXL6_SECT l0 op r0 lb e1 e2 rb ->
            INFIXL6_SECT l0 op r0 lb <$> wExpr e1 <*> wExpr e2 <*> pure rb
        INFIXR7_SECT l0 op r0 lb e1 e2 rb ->
            INFIXR7_SECT l0 op r0 lb <$> wExpr e1 <*> wExpr e2 <*> pure rb
        INFIXL8_SECT l0 op r0 lb e1 e2 rb ->
            INFIXL8_SECT l0 op r0 lb <$> wExpr e1 <*> wExpr e2 <*> pure rb
        INFIXPR_SECT p lb e1 e2 rb ->
            INFIXPR_SECT p lb <$> wExpr e1 <*> wExpr e2 <*> pure rb
        INFIXTN_SECT t lb e1 e2 rb ->
            INFIXTN_SECT t lb <$> wExpr e1 <*> wExpr e2 <*> pure rb
        LIST_EXPR lb es rb -> LIST_EXPR lb <$> traverse wExpr es <*> pure rb
        VAR_EXPR{} -> pure e0
        INT_EXPR{} -> pure e0
        STRING_EXPR{} -> pure e0
        CHAR_EXPR{} -> pure e0
        DOUBLE_EXPR{} -> pure e0
        UNIT_EXPR{} -> pure e0
        FOLD_EXPR e ps -> FOLD_EXPR <$> wExpr e <*> traverse wFEP ps
        UNFOLD_EXPR e ps -> UNFOLD_EXPR <$> wExpr e <*> traverse wUEP ps
        CASE_EXPR c e ps -> CASE_EXPR c <$> wExpr e <*> traverse wPEP ps
        SWITCH_EXP ps -> SWITCH_EXP <$> traverse wSEP ps
        STORE_EXPR s lb np rb -> STORE_EXPR s lb <$> wNp np <*> pure rb
        DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR i lb es rb ->
            DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR i lb
                <$> traverse wExpr es <*> pure rb
        DESTRUCTOR_CONSTRUCTOR_NO_ARGS_EXPR{} -> pure e0
        TUPLE_EXPR lb e tels rb ->
            TUPLE_EXPR lb <$> wExpr e <*> traverse wTEL tels <*> pure rb
        FUN_EXPR i lb es rb ->
            noteUse i >> FUN_EXPR i lb <$> traverse wExpr es <*> pure rb
        RECORD_EXPR lb ps rb ->
            RECORD_EXPR lb <$> traverse wREP ps <*> pure rb
        FUNQ_EXPR m f lb es rb ->
            FUNQ_EXPR m f lb <$> traverse wExpr es <*> pure rb
        BRACKETED_EXPR lb e rb ->
            BRACKETED_EXPR lb <$> wExpr e <*> pure rb
