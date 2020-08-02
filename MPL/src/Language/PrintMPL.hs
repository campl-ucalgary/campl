{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Language.PrintMPL where

-- pretty-printer generated by the BNF converter

import Language.AbsMPL
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)



instance Print UIdent where
  prt _ (UIdent (_,i)) = doc (showString ( i))


instance Print PIdent where
  prt _ (PIdent (_,i)) = doc (showString ( i))
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])

instance Print UPIdent where
  prt _ (UPIdent (_,i)) = doc (showString ( i))


instance Print PInteger where
  prt _ (PInteger (_,i)) = doc (showString ( i))


instance Print Par where
  prt _ (Par (_,i)) = doc (showString ( i))


instance Print Tensor where
  prt _ (Tensor (_,i)) = doc (showString ( i))


instance Print LBracket where
  prt _ (LBracket (_,i)) = doc (showString ( i))


instance Print RBracket where
  prt _ (RBracket (_,i)) = doc (showString ( i))


instance Print LSquareBracket where
  prt _ (LSquareBracket (_,i)) = doc (showString ( i))


instance Print RSquareBracket where
  prt _ (RSquareBracket (_,i)) = doc (showString ( i))


instance Print NullPattern where
  prt _ (NullPattern (_,i)) = doc (showString ( i))


instance Print Colon where
  prt _ (Colon (_,i)) = doc (showString ( i))


instance Print Infixl1op where
  prt _ (Infixl1op i) = doc (showString ( i))


instance Print Infixl2op where
  prt _ (Infixl2op i) = doc (showString ( i))


instance Print Infixl3op where
  prt _ (Infixl3op i) = doc (showString ( i))


instance Print Infixl4op where
  prt _ (Infixl4op i) = doc (showString ( i))


instance Print Infixl5op where
  prt _ (Infixl5op i) = doc (showString ( i))


instance Print Infixl6op where
  prt _ (Infixl6op i) = doc (showString ( i))


instance Print Infixr7op where
  prt _ (Infixr7op i) = doc (showString ( i))


instance Print Infixl8op where
  prt _ (Infixl8op i) = doc (showString ( i))



instance Print MplProg where
  prt i e = case e of
    MPL_PROG mplstmts -> prPrec i 0 (concatD [prt 0 mplstmts])

instance Print MplStmt where
  prt i e = case e of
    MPL_DEFN_STMS_WHERE mpldefns mplstmts -> prPrec i 0 (concatD [doc (showString "defn"), doc (showString "{"), prt 0 mpldefns, doc (showString "}"), doc (showString "where"), doc (showString "{"), prt 0 mplstmts, doc (showString "}")])
    MPL_DEFN_STMS mpldefns -> prPrec i 0 (concatD [doc (showString "defn"), doc (showString "{"), prt 0 mpldefns, doc (showString "}")])
    MPL_STMT mpldefn -> prPrec i 0 (concatD [prt 0 mpldefn])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print MplDefn where
  prt i e = case e of
    MPL_SEQUENTIAL_TYPE_DEFN sequentialtypedefn -> prPrec i 0 (concatD [prt 0 sequentialtypedefn])
    MPL_CONCURRENT_TYPE_DEFN concurrenttypedefn -> prPrec i 0 (concatD [prt 0 concurrenttypedefn])
    MPL_FUNCTION_DEFN functiondefn -> prPrec i 0 (concatD [prt 0 functiondefn])
    MPL_PROCESS_DEFN processdefn -> prPrec i 0 (concatD [prt 0 processdefn])
    MPL_DEFNTEST -> prPrec i 0 (concatD [doc (showString "potato")])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print MplType where
  prt i e = case e of
    MPL_TYPE mpltype -> prPrec i 0 (concatD [prt 0 mpltype])
    PAR_TYPE mpltype1 par mpltype2 -> prPrec i 0 (concatD [prt 1 mpltype1, prt 0 par, prt 1 mpltype2])
    TENSOR_TYPE mpltype1 tensor mpltype2 -> prPrec i 1 (concatD [prt 2 mpltype1, prt 0 tensor, prt 2 mpltype2])
    GETPUT_TYPE uident lbracket mpltype1 mpltype2 rbracket -> prPrec i 2 (concatD [prt 0 uident, prt 0 lbracket, prt 0 mpltype1, doc (showString "|"), prt 0 mpltype2, prt 0 rbracket])
    MPL_UIDENT_ARGS_TYPE uident lbracket mpltypes rbracket -> prPrec i 3 (concatD [prt 0 uident, prt 0 lbracket, prt 0 mpltypes, prt 0 rbracket])
    MPL_UIDENT_NO_ARGS_TYPE uident -> prPrec i 3 (concatD [prt 0 uident])
    MPL_UNIT_TYPE lbracket rbracket -> prPrec i 3 (concatD [prt 0 lbracket, prt 0 rbracket])
    MPL_BRACKETED_TYPE lbracket mpltype rbracket -> prPrec i 3 (concatD [prt 0 lbracket, prt 0 mpltype, prt 0 rbracket])
    MPL_LIST_TYPE lsquarebracket mpltype rsquarebracket -> prPrec i 3 (concatD [prt 0 lsquarebracket, prt 0 mpltype, prt 0 rsquarebracket])
    MPL_TUPLE_TYPE lbracket mpltype tuplelisttypes rbracket -> prPrec i 3 (concatD [prt 0 lbracket, prt 0 mpltype, doc (showString ","), prt 0 tuplelisttypes, prt 0 rbracket])
    MPL_SEQ_ARROW_TYPE forallvarlists mpltypes mpltype -> prPrec i 3 (concatD [doc (showString "forall"), prt 0 forallvarlists, doc (showString "."), prt 0 mpltypes, doc (showString "->"), prt 0 mpltype])
    MPL_CONC_ARROW_TYPE forallvarlists mpltypes1 mpltypes2 mpltypes3 -> prPrec i 3 (concatD [doc (showString "forall"), prt 0 forallvarlists, doc (showString "."), prt 0 mpltypes1, doc (showString "|"), prt 0 mpltypes2, doc (showString "=>"), prt 0 mpltypes3])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print TupleListType where
  prt i e = case e of
    TUPLE_LIST_TYPE mpltype -> prPrec i 0 (concatD [prt 0 mpltype])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print ForallVarList where
  prt i e = case e of
    MPL_SEQ_FUN_TYPE_FORALL_LIST uident -> prPrec i 0 (concatD [prt 0 uident])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print SequentialTypeDefn where
  prt i e = case e of
    DATA_DEFN seqtypeclausedefns -> prPrec i 0 (concatD [doc (showString "data"), prt 0 seqtypeclausedefns])
    CODATA_DEFN seqtypeclausedefns -> prPrec i 0 (concatD [doc (showString "codata"), prt 0 seqtypeclausedefns])

instance Print SeqTypeClauseDefn where
  prt i e = case e of
    SEQ_TYPE_CLAUSE mpltype1 mpltype2 seqtypephrasedefns -> prPrec i 0 (concatD [prt 0 mpltype1, doc (showString "->"), prt 0 mpltype2, doc (showString "="), doc (showString "{"), prt 0 seqtypephrasedefns, doc (showString "}")])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "and"), prt 0 xs])
instance Print SeqTypePhraseDefn where
  prt i e = case e of
    SEQ_TYPE_PHRASE typehandlenames mpltypes mpltype -> prPrec i 0 (concatD [prt 0 typehandlenames, doc (showString "::"), prt 0 mpltypes, doc (showString "->"), prt 0 mpltype])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print ConcurrentTypeDefn where
  prt i e = case e of
    PROTOCOL_DEFN concurrenttypeclausedefns -> prPrec i 0 (concatD [doc (showString "protocol"), prt 0 concurrenttypeclausedefns])
    COPROTOCOL_DEFN concurrenttypeclausedefns -> prPrec i 0 (concatD [doc (showString "coprotocol"), prt 0 concurrenttypeclausedefns])

instance Print ConcurrentTypeClauseDefn where
  prt i e = case e of
    CONCURRENT_TYPE_CLAUSE mpltype1 mpltype2 concurrenttypephrasedefns -> prPrec i 0 (concatD [prt 0 mpltype1, doc (showString "=>"), prt 0 mpltype2, doc (showString "="), doc (showString "{"), prt 0 concurrenttypephrasedefns, doc (showString "}")])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString "and"), prt 0 xs])
instance Print ConcurrentTypePhraseDefn where
  prt i e = case e of
    CONCURRENT_TYPE_PHRASE typehandlenames mpltype1 mpltype2 -> prPrec i 0 (concatD [prt 0 typehandlenames, doc (showString "::"), prt 0 mpltype1, doc (showString "=>"), prt 0 mpltype2])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print TypeHandleName where
  prt i e = case e of
    TYPE_HANDLE_NAME uident -> prPrec i 0 (concatD [prt 0 uident])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Expr where
  prt i e = case e of
    EXPR expr -> prPrec i 0 (concatD [prt 0 expr])
    TYPED_EXPR expr mpltype -> prPrec i 0 (concatD [prt 0 expr, doc (showString "::"), prt 0 mpltype])
    IF_EXPR expr1 expr2 expr3 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 expr1, doc (showString "then"), prt 0 expr2, doc (showString "else"), prt 0 expr3])
    LET_EXPR letexprphrases expr -> prPrec i 0 (concatD [doc (showString "let"), doc (showString "{"), prt 0 letexprphrases, doc (showString "}"), doc (showString "in"), prt 0 expr])
    INFIXR0_EXPR expr1 colon expr2 -> prPrec i 0 (concatD [prt 1 expr1, prt 0 colon, prt 0 expr2])
    INFIXL1_EXPR expr1 infixlop expr2 -> prPrec i 1 (concatD [prt 1 expr1, prt 0 infixlop, prt 2 expr2])
    INFIXL2_EXPR expr1 infixlop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 infixlop, prt 3 expr2])
    INFIXL3_EXPR expr1 infixlop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 infixlop, prt 4 expr2])
    INFIXL4_EXPR expr1 infixlop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 infixlop, prt 5 expr2])
    INFIXL5_EXPR expr1 infixlop expr2 -> prPrec i 5 (concatD [prt 5 expr1, prt 0 infixlop, prt 6 expr2])
    INFIXL6_EXPR expr1 infixlop expr2 -> prPrec i 6 (concatD [prt 6 expr1, prt 0 infixlop, prt 7 expr2])
    INFIXR7_EXPR expr1 infixrop expr2 -> prPrec i 7 (concatD [prt 8 expr1, prt 0 infixrop, prt 7 expr2])
    INFIXL8_EXPR expr1 infixlop expr2 -> prPrec i 8 (concatD [prt 8 expr1, prt 0 infixlop, prt 10 expr2])
    LIST_EXPR lsquarebracket exprs rsquarebracket -> prPrec i 10 (concatD [prt 0 lsquarebracket, prt 0 exprs, prt 0 rsquarebracket])
    VAR_EXPR pident -> prPrec i 10 (concatD [prt 0 pident])
    INT_EXPR pinteger -> prPrec i 10 (concatD [prt 0 pinteger])
    STRING_EXPR str -> prPrec i 10 (concatD [prt 0 str])
    CHAR_EXPR c -> prPrec i 10 (concatD [prt 0 c])
    DOUBLE_EXPR d -> prPrec i 10 (concatD [prt 0 d])
    UNIT_EXPR lbracket rbracket -> prPrec i 10 (concatD [prt 0 lbracket, prt 0 rbracket])
    FOLD_EXPR expr foldexprphrases -> prPrec i 10 (concatD [doc (showString "fold"), prt 0 expr, doc (showString "of"), doc (showString "{"), prt 0 foldexprphrases, doc (showString "}")])
    UNFOLD_EXPR expr unfoldexprphrases -> prPrec i 10 (concatD [doc (showString "unfold"), prt 0 expr, doc (showString "of"), doc (showString "{"), prt 0 unfoldexprphrases, doc (showString "}")])
    CASE_EXPR expr pattexprphrases -> prPrec i 10 (concatD [doc (showString "case"), prt 0 expr, doc (showString "of"), doc (showString "{"), prt 0 pattexprphrases, doc (showString "}")])
    SWITCH_EXP switchexprphrases -> prPrec i 10 (concatD [doc (showString "switch"), doc (showString "{"), prt 0 switchexprphrases, doc (showString "}")])
    DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR uident lbracket exprs rbracket -> prPrec i 10 (concatD [prt 0 uident, prt 0 lbracket, prt 0 exprs, prt 0 rbracket])
    DESTRUCTOR_CONSTRUCTOR_NO_ARGS_EXPR uident -> prPrec i 10 (concatD [prt 0 uident])
    TUPLE_EXPR lbracket expr tupleexprlists rbracket -> prPrec i 10 (concatD [prt 0 lbracket, prt 0 expr, doc (showString ","), prt 0 tupleexprlists, prt 0 rbracket])
    FUN_EXPR pident lbracket exprs rbracket -> prPrec i 10 (concatD [prt 0 pident, prt 0 lbracket, prt 0 exprs, prt 0 rbracket])
    RECORD_EXPR lbracket recordexprphrases rbracket -> prPrec i 10 (concatD [prt 0 lbracket, prt 0 recordexprphrases, prt 0 rbracket])
    BRACKETED_EXPR lbracket expr rbracket -> prPrec i 10 (concatD [prt 0 lbracket, prt 0 expr, prt 0 rbracket])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print UnfoldExprPhrase where
  prt i e = case e of
    UNFOLD_EXPR_PHRASE pattern foldexprphrases -> prPrec i 0 (concatD [prt 0 pattern, doc (showString "of"), doc (showString "{"), prt 0 foldexprphrases, doc (showString "}")])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print FoldExprPhrase where
  prt i e = case e of
    FOLD_EXPR_PHRASE uident colon patterns expr -> prPrec i 0 (concatD [prt 0 uident, prt 0 colon, prt 0 patterns, doc (showString "->"), prt 0 expr])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print LetExprPhrase where
  prt i e = case e of
    LET_EXPR_PHRASE mplstmt -> prPrec i 0 (concatD [prt 0 mplstmt])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print TupleExprList where
  prt i e = case e of
    TUPLE_EXPR_LIST expr -> prPrec i 0 (concatD [prt 0 expr])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print RecordExprPhrase where
  prt i e = case e of
    RECORD_EXPR_PHRASE uident expr -> prPrec i 0 (concatD [prt 0 uident, doc (showString ":="), prt 0 expr])
    RECORD_EXPR_HIGHER_ORDER_PHRASE uident pattexprphrase -> prPrec i 0 (concatD [prt 0 uident, doc (showString ":="), prt 0 pattexprphrase])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print SwitchExprPhrase where
  prt i e = case e of
    SWITCH_EXPR_PHRASE expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "->"), prt 0 expr2])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print PattExprPhrase where
  prt i e = case e of
    PATTERN_TO_EXPR patterns expr -> prPrec i 0 (concatD [prt 0 patterns, doc (showString "->"), prt 0 expr])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print Pattern where
  prt i e = case e of
    PATTERN pattern -> prPrec i 0 (concatD [prt 0 pattern])
    TYPED_PATTERN pattern mpltype -> prPrec i 0 (concatD [prt 0 pattern, doc (showString "::"), prt 0 mpltype])
    LIST_COLON_PATTERN pattern1 colon pattern2 -> prPrec i 0 (concatD [prt 1 pattern1, prt 0 colon, prt 0 pattern2])
    CONSTRUCTOR_PATTERN_ARGS uident lbracket patterns rbracket -> prPrec i 1 (concatD [prt 0 uident, prt 0 lbracket, prt 0 patterns, prt 0 rbracket])
    CONSTRUCTOR_PATTERN_NO_ARGS uident -> prPrec i 1 (concatD [prt 0 uident])
    UNIT_PATTERN lbracket rbracket -> prPrec i 1 (concatD [prt 0 lbracket, prt 0 rbracket])
    RECORD_PATTERN lbracket destructorpatternphrases rbracket -> prPrec i 1 (concatD [prt 0 lbracket, prt 0 destructorpatternphrases, prt 0 rbracket])
    LIST_PATTERN lsquarebracket patterns rsquarebracket -> prPrec i 1 (concatD [prt 0 lsquarebracket, prt 0 patterns, prt 0 rsquarebracket])
    TUPLE_PATTERN lbracket pattern tuplelistpatterns rbracket -> prPrec i 1 (concatD [prt 0 lbracket, prt 0 pattern, doc (showString ","), prt 0 tuplelistpatterns, prt 0 rbracket])
    VAR_PATTERN pident -> prPrec i 1 (concatD [prt 0 pident])
    STR_PATTERN str -> prPrec i 1 (concatD [prt 0 str])
    INT_PATTERN pinteger -> prPrec i 1 (concatD [prt 0 pinteger])
    NULL_PATTERN nullpattern -> prPrec i 1 (concatD [prt 0 nullpattern])
    BRACKETED_PATTERN lbracket pattern rbracket -> prPrec i 1 (concatD [prt 0 lbracket, prt 0 pattern, prt 0 rbracket])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print TupleListPattern where
  prt i e = case e of
    TUPLE_LIST_PATTERN pattern -> prPrec i 0 (concatD [prt 0 pattern])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print DestructorPatternPhrase where
  prt i e = case e of
    DESTRUCTOR_PATTERN_PHRASE uident pattern -> prPrec i 0 (concatD [prt 0 uident, doc (showString ":="), prt 0 pattern])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print FunctionDefn where
  prt i e = case e of
    INTERNAL_TYPED_FUNCTION_DEFN pident mpltype pattexprphrases -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 pident, doc (showString "::"), prt 0 mpltype, doc (showString "="), doc (showString "{"), prt 0 pattexprphrases, doc (showString "}")])
    TYPED_FUNCTION_DEFN pident mpltypes mpltype pattexprphrases -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 pident, doc (showString "::"), prt 0 mpltypes, doc (showString "->"), prt 0 mpltype, doc (showString "="), doc (showString "{"), prt 0 pattexprphrases, doc (showString "}")])
    FUNCTION_DEFN pident pattexprphrases -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 pident, doc (showString "="), doc (showString "{"), prt 0 pattexprphrases, doc (showString "}")])

instance Print ProcessDefn where
  prt i e = case e of
    TYPED_PROCESS_DEFN pident mpltypes1 mpltypes2 mpltypes3 processphrases -> prPrec i 0 (concatD [doc (showString "proc"), prt 0 pident, doc (showString "::"), prt 0 mpltypes1, doc (showString "|"), prt 0 mpltypes2, doc (showString "=>"), prt 0 mpltypes3, doc (showString "="), doc (showString "{"), prt 0 processphrases, doc (showString "}")])
    PROCESS_DEFN pident processphrases -> prPrec i 0 (concatD [doc (showString "proc"), prt 0 pident, doc (showString "="), doc (showString "{"), prt 0 processphrases, doc (showString "}")])

instance Print ProcessPhrase where
  prt i e = case e of
    PROCESS_PHRASE patterns pidents1 pidents2 processcommandsblock -> prPrec i 0 (concatD [prt 0 patterns, doc (showString "|"), prt 0 pidents1, doc (showString "=>"), prt 0 pidents2, doc (showString "->"), prt 0 processcommandsblock])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print ProcessCommandsBlock where
  prt i e = case e of
    PROCESS_COMMANDS_DO_BLOCK processcommands -> prPrec i 0 (concatD [doc (showString "do"), doc (showString "{"), prt 0 processcommands, doc (showString "}")])
    PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK processcommand -> prPrec i 0 (concatD [prt 0 processcommand])

instance Print ProcessCommand where
  prt i e = case e of
    PROCESS_RUN pident lbracket exprs pidents1 pidents2 rbracket -> prPrec i 0 (concatD [prt 0 pident, prt 0 lbracket, prt 0 exprs, doc (showString "|"), prt 0 pidents1, doc (showString "=>"), prt 0 pidents2, prt 0 rbracket])
    PROCESS_CLOSE pident -> prPrec i 0 (concatD [doc (showString "close"), prt 0 pident])
    PROCESS_HALT pident -> prPrec i 0 (concatD [doc (showString "halt"), prt 0 pident])
    PROCESS_GET pattern pident -> prPrec i 0 (concatD [doc (showString "get"), prt 0 pattern, doc (showString "on"), prt 0 pident])
    PROCESS_PUT expr pident -> prPrec i 0 (concatD [doc (showString "put"), prt 0 expr, doc (showString "on"), prt 0 pident])
    PROCESS_HCASE pident hcasephrases -> prPrec i 0 (concatD [doc (showString "hcase"), prt 0 pident, doc (showString "of"), doc (showString "{"), prt 0 hcasephrases, doc (showString "}")])
    PROCESS_HPUT uident pident -> prPrec i 0 (concatD [doc (showString "hput"), prt 0 uident, doc (showString "on"), prt 0 pident])
    PROCESS_SPLIT pident splitchannels -> prPrec i 0 (concatD [doc (showString "split"), prt 0 pident, doc (showString "into"), prt 0 splitchannels])
    PROCESS_FORK pident forkphrases -> prPrec i 0 (concatD [doc (showString "fork"), prt 0 pident, doc (showString "as"), doc (showString "{"), prt 0 forkphrases, doc (showString "}")])
    PROCESS_ID pident1 pident2 -> prPrec i 0 (concatD [prt 0 pident1, doc (showString "|=|"), prt 0 pident2])
    PROCESS_NEG pident1 pident2 -> prPrec i 0 (concatD [prt 0 pident1, doc (showString "|=|"), doc (showString "neg"), prt 0 pident2])
    PROCESS_RACE racephrases -> prPrec i 0 (concatD [doc (showString "race"), doc (showString "{"), prt 0 racephrases, doc (showString "}")])
    PROCESS_PLUG plugphrases -> prPrec i 0 (concatD [doc (showString "plug"), doc (showString "{"), prt 0 plugphrases, doc (showString "}")])
    PROCESS_CASE expr processcasephrases -> prPrec i 0 (concatD [doc (showString "case"), prt 0 expr, doc (showString "of"), doc (showString "{"), prt 0 processcasephrases, doc (showString "}")])
    PROCESS_SWITCH processswitchphrases -> prPrec i 0 (concatD [doc (showString "switch"), doc (showString "{"), prt 0 processswitchphrases, doc (showString "}")])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print HCasePhrase where
  prt i e = case e of
    HCASE_PHRASE uident processcommandsblock -> prPrec i 0 (concatD [prt 0 uident, doc (showString "->"), prt 0 processcommandsblock])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print SplitChannel where
  prt i e = case e of
    SPLIT_CHANNEL pident -> prPrec i 0 (concatD [prt 0 pident])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print ForkPhrase where
  prt i e = case e of
    FORK_PHRASE pident processcommandsblock -> prPrec i 0 (concatD [prt 0 pident, doc (showString "->"), prt 0 processcommandsblock])
    FORK_WITH_PHRASE pident forkchannels processcommandsblock -> prPrec i 0 (concatD [prt 0 pident, doc (showString "with"), prt 0 forkchannels, doc (showString "->"), prt 0 processcommandsblock])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print ForkChannel where
  prt i e = case e of
    FORK_CHANNEL pident -> prPrec i 0 (concatD [prt 0 pident])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print RacePhrase where
  prt i e = case e of
    RACE_PHRASE pident processcommandsblock -> prPrec i 0 (concatD [prt 0 pident, doc (showString "->"), prt 0 processcommandsblock])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print PlugPhrase where
  prt i e = case e of
    PLUG_PHRASE processcommandsblock -> prPrec i 0 (concatD [prt 0 processcommandsblock])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print ProcessCasePhrase where
  prt i e = case e of
    PROCESS_CASE_PHRASE pattern processcommandsblock -> prPrec i 0 (concatD [prt 0 pattern, doc (showString "->"), prt 0 processcommandsblock])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])
instance Print ProcessSwitchPhrase where
  prt i e = case e of
    PROCESS_SWITCH_PHRASE expr processcommandsblock -> prPrec i 0 (concatD [prt 0 expr, doc (showString "->"), prt 0 processcommandsblock])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])

