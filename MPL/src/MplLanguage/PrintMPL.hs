-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for MplLanguage.

module MplLanguage.PrintMPL where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified MplLanguage.AbsMPL

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print MplLanguage.AbsMPL.PInteger where
  prt _ (MplLanguage.AbsMPL.PInteger (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.PDouble where
  prt _ (MplLanguage.AbsMPL.PDouble (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.PChar where
  prt _ (MplLanguage.AbsMPL.PChar (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.PString where
  prt _ (MplLanguage.AbsMPL.PString (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Par where
  prt _ (MplLanguage.AbsMPL.Par (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Tensor where
  prt _ (MplLanguage.AbsMPL.Tensor (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.LBracket where
  prt _ (MplLanguage.AbsMPL.LBracket (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.RBracket where
  prt _ (MplLanguage.AbsMPL.RBracket (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.LSquareBracket where
  prt _ (MplLanguage.AbsMPL.LSquareBracket (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.RSquareBracket where
  prt _ (MplLanguage.AbsMPL.RSquareBracket (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.NullPattern where
  prt _ (MplLanguage.AbsMPL.NullPattern (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Colon where
  prt _ (MplLanguage.AbsMPL.Colon (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Infixl1op where
  prt _ (MplLanguage.AbsMPL.Infixl1op (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Infixl2op where
  prt _ (MplLanguage.AbsMPL.Infixl2op (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Infixl3op where
  prt _ (MplLanguage.AbsMPL.Infixl3op (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Infixl4op where
  prt _ (MplLanguage.AbsMPL.Infixl4op (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Infixl5op where
  prt _ (MplLanguage.AbsMPL.Infixl5op (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Infixl6op where
  prt _ (MplLanguage.AbsMPL.Infixl6op (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Infixr7op where
  prt _ (MplLanguage.AbsMPL.Infixr7op (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Infixl8op where
  prt _ (MplLanguage.AbsMPL.Infixl8op (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.InfixU1op where
  prt _ (MplLanguage.AbsMPL.InfixU1op (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.InfixU2op where
  prt _ (MplLanguage.AbsMPL.InfixU2op (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.InfixU3op where
  prt _ (MplLanguage.AbsMPL.InfixU3op (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.InfixU5op where
  prt _ (MplLanguage.AbsMPL.InfixU5op (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.InfixU6op where
  prt _ (MplLanguage.AbsMPL.InfixU6op (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.InfixU7op where
  prt _ (MplLanguage.AbsMPL.InfixU7op (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Close where
  prt _ (MplLanguage.AbsMPL.Close (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Halt where
  prt _ (MplLanguage.AbsMPL.Halt (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Get where
  prt _ (MplLanguage.AbsMPL.Get (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Put where
  prt _ (MplLanguage.AbsMPL.Put (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.HCase where
  prt _ (MplLanguage.AbsMPL.HCase (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.HPut where
  prt _ (MplLanguage.AbsMPL.HPut (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Split where
  prt _ (MplLanguage.AbsMPL.Split (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Fork where
  prt _ (MplLanguage.AbsMPL.Fork (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.ChId where
  prt _ (MplLanguage.AbsMPL.ChId (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.Case where
  prt _ (MplLanguage.AbsMPL.Case (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.UIdent where
  prt _ (MplLanguage.AbsMPL.UIdent (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.PIdent where
  prt _ (MplLanguage.AbsMPL.PIdent (_,i)) = doc $ showString i
instance Print MplLanguage.AbsMPL.UPIdent where
  prt _ (MplLanguage.AbsMPL.UPIdent (_,i)) = doc $ showString i
instance Print [MplLanguage.AbsMPL.PIdent] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print MplLanguage.AbsMPL.MplProg where
  prt i = \case
    MplLanguage.AbsMPL.MPL_PROG mplstmts -> prPrec i 0 (concatD [prt 0 mplstmts])

instance Print MplLanguage.AbsMPL.MplStmt where
  prt i = \case
    MplLanguage.AbsMPL.MPL_DEFN_STMS_WHERE mpldefns mplwheres -> prPrec i 0 (concatD [doc (showString "defn"), doc (showString "{"), prt 0 mpldefns, doc (showString "}"), doc (showString "where"), doc (showString "{"), prt 0 mplwheres, doc (showString "}")])
    MplLanguage.AbsMPL.MPL_DEFN_STMS mpldefns -> prPrec i 0 (concatD [doc (showString "defn"), doc (showString "{"), prt 0 mpldefns, doc (showString "}")])
    MplLanguage.AbsMPL.MPL_STMT mpldefn -> prPrec i 0 (concatD [prt 0 mpldefn])

instance Print [MplLanguage.AbsMPL.MplDefn] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [MplLanguage.AbsMPL.MplStmt] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print MplLanguage.AbsMPL.MplWhere where
  prt i = \case
    MplLanguage.AbsMPL.MPL_WHERE mplstmt -> prPrec i 0 (concatD [prt 0 mplstmt])

instance Print [MplLanguage.AbsMPL.MplWhere] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print MplLanguage.AbsMPL.MplDefn where
  prt i = \case
    MplLanguage.AbsMPL.MPL_SEQUENTIAL_TYPE_DEFN sequentialtypedefn -> prPrec i 0 (concatD [prt 0 sequentialtypedefn])
    MplLanguage.AbsMPL.MPL_CONCURRENT_TYPE_DEFN concurrenttypedefn -> prPrec i 0 (concatD [prt 0 concurrenttypedefn])
    MplLanguage.AbsMPL.MPL_FUNCTION_DEFN functiondefn -> prPrec i 0 (concatD [prt 0 functiondefn])
    MplLanguage.AbsMPL.MPL_PROCESS_DEFN processdefn -> prPrec i 0 (concatD [prt 0 processdefn])
    MplLanguage.AbsMPL.MPL_DEFNTEST -> prPrec i 0 (concatD [doc (showString "potato")])

instance Print MplLanguage.AbsMPL.MplType where
  prt i = \case
    MplLanguage.AbsMPL.MPL_TYPE mpltype -> prPrec i 0 (concatD [prt 0 mpltype])
    MplLanguage.AbsMPL.PAR_TYPE mpltype1 par mpltype2 -> prPrec i 0 (concatD [prt 1 mpltype1, prt 0 par, prt 1 mpltype2])
    MplLanguage.AbsMPL.TENSOR_TYPE mpltype1 tensor mpltype2 -> prPrec i 1 (concatD [prt 2 mpltype1, prt 0 tensor, prt 2 mpltype2])
    MplLanguage.AbsMPL.MPL_UIDENT_ARGS_TYPE uident lbracket mpltypes rbracket -> prPrec i 2 (concatD [prt 0 uident, prt 0 lbracket, prt 0 mpltypes, prt 0 rbracket])
    MplLanguage.AbsMPL.MPL_UIDENT_SEQ_CONC_ARGS_TYPE uident lbracket mpltypes1 mpltypes2 rbracket -> prPrec i 2 (concatD [prt 0 uident, prt 0 lbracket, prt 0 mpltypes1, doc (showString "|"), prt 0 mpltypes2, prt 0 rbracket])
    MplLanguage.AbsMPL.MPL_UIDENT_NO_ARGS_TYPE uident -> prPrec i 2 (concatD [prt 0 uident])
    MplLanguage.AbsMPL.MPL_UNIT_TYPE lbracket rbracket -> prPrec i 2 (concatD [prt 0 lbracket, prt 0 rbracket])
    MplLanguage.AbsMPL.MPL_BRACKETED_TYPE lbracket mpltype rbracket -> prPrec i 2 (concatD [prt 0 lbracket, prt 0 mpltype, prt 0 rbracket])
    MplLanguage.AbsMPL.MPL_LIST_TYPE lsquarebracket mpltype rsquarebracket -> prPrec i 2 (concatD [prt 0 lsquarebracket, prt 0 mpltype, prt 0 rsquarebracket])
    MplLanguage.AbsMPL.MPL_TUPLE_TYPE lbracket mpltype tuplelisttypes rbracket -> prPrec i 2 (concatD [prt 0 lbracket, prt 0 mpltype, doc (showString ","), prt 0 tuplelisttypes, prt 0 rbracket])
    MplLanguage.AbsMPL.MPL_SEQ_ARROW_TYPE forallvarlists mpltypes mpltype -> prPrec i 2 (concatD [doc (showString "forall"), prt 0 forallvarlists, doc (showString "."), prt 0 mpltypes, doc (showString "->"), prt 0 mpltype])
    MplLanguage.AbsMPL.MPL_CONC_ARROW_TYPE forallvarlists mpltypes1 mpltypes2 mpltypes3 -> prPrec i 2 (concatD [doc (showString "forall"), prt 0 forallvarlists, doc (showString "."), prt 0 mpltypes1, doc (showString "|"), prt 0 mpltypes2, doc (showString "=>"), prt 0 mpltypes3])

instance Print MplLanguage.AbsMPL.TupleListType where
  prt i = \case
    MplLanguage.AbsMPL.TUPLE_LIST_TYPE mpltype -> prPrec i 0 (concatD [prt 0 mpltype])

instance Print MplLanguage.AbsMPL.ForallVarList where
  prt i = \case
    MplLanguage.AbsMPL.MPL_SEQ_FUN_TYPE_FORALL_LIST uident -> prPrec i 0 (concatD [prt 0 uident])

instance Print [MplLanguage.AbsMPL.ForallVarList] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString " "), prt 0 xs]

instance Print [MplLanguage.AbsMPL.TupleListType] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [MplLanguage.AbsMPL.MplType] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print MplLanguage.AbsMPL.SequentialTypeDefn where
  prt i = \case
    MplLanguage.AbsMPL.DATA_DEFN seqtypeclausedefns -> prPrec i 0 (concatD [doc (showString "data"), prt 0 seqtypeclausedefns])
    MplLanguage.AbsMPL.CODATA_DEFN seqtypeclausedefns -> prPrec i 0 (concatD [doc (showString "codata"), prt 0 seqtypeclausedefns])

instance Print MplLanguage.AbsMPL.SeqTypeClauseDefn where
  prt i = \case
    MplLanguage.AbsMPL.SEQ_TYPE_CLAUSE mpltype1 mpltype2 seqtypephrasedefns -> prPrec i 0 (concatD [prt 0 mpltype1, doc (showString "->"), prt 0 mpltype2, doc (showString "="), doc (showString "{"), prt 0 seqtypephrasedefns, doc (showString "}")])

instance Print MplLanguage.AbsMPL.SeqTypePhraseDefn where
  prt i = \case
    MplLanguage.AbsMPL.SEQ_TYPE_PHRASE typehandlenames mpltypes mpltype -> prPrec i 0 (concatD [prt 0 typehandlenames, doc (showString "::"), prt 0 mpltypes, doc (showString "->"), prt 0 mpltype])

instance Print [MplLanguage.AbsMPL.SeqTypeClauseDefn] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "and"), prt 0 xs]

instance Print [MplLanguage.AbsMPL.SeqTypePhraseDefn] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print MplLanguage.AbsMPL.ConcurrentTypeDefn where
  prt i = \case
    MplLanguage.AbsMPL.PROTOCOL_DEFN concurrenttypeclausedefns -> prPrec i 0 (concatD [doc (showString "protocol"), prt 0 concurrenttypeclausedefns])
    MplLanguage.AbsMPL.COPROTOCOL_DEFN concurrenttypeclausedefns -> prPrec i 0 (concatD [doc (showString "coprotocol"), prt 0 concurrenttypeclausedefns])

instance Print MplLanguage.AbsMPL.ConcurrentTypeClauseDefn where
  prt i = \case
    MplLanguage.AbsMPL.CONCURRENT_TYPE_CLAUSE mpltype1 mpltype2 concurrenttypephrasedefns -> prPrec i 0 (concatD [prt 0 mpltype1, doc (showString "=>"), prt 0 mpltype2, doc (showString "="), doc (showString "{"), prt 0 concurrenttypephrasedefns, doc (showString "}")])

instance Print MplLanguage.AbsMPL.ConcurrentTypePhraseDefn where
  prt i = \case
    MplLanguage.AbsMPL.CONCURRENT_TYPE_PHRASE typehandlenames mpltype1 mpltype2 -> prPrec i 0 (concatD [prt 0 typehandlenames, doc (showString "::"), prt 0 mpltype1, doc (showString "=>"), prt 0 mpltype2])

instance Print [MplLanguage.AbsMPL.ConcurrentTypeClauseDefn] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "and"), prt 0 xs]

instance Print [MplLanguage.AbsMPL.ConcurrentTypePhraseDefn] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print MplLanguage.AbsMPL.TypeHandleName where
  prt i = \case
    MplLanguage.AbsMPL.TYPE_HANDLE_NAME uident -> prPrec i 0 (concatD [prt 0 uident])

instance Print [MplLanguage.AbsMPL.TypeHandleName] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print MplLanguage.AbsMPL.Expr where
  prt i = \case
    MplLanguage.AbsMPL.EXPR expr -> prPrec i 0 (concatD [prt 0 expr])
    MplLanguage.AbsMPL.TYPED_EXPR expr mpltype -> prPrec i 0 (concatD [prt 0 expr, doc (showString "::"), prt 0 mpltype])
    MplLanguage.AbsMPL.IF_EXPR expr1 expr2 expr3 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 expr1, doc (showString "then"), prt 0 expr2, doc (showString "else"), prt 0 expr3])
    MplLanguage.AbsMPL.LET_EXPR letexprphrases expr -> prPrec i 0 (concatD [doc (showString "let"), doc (showString "{"), prt 0 letexprphrases, doc (showString "}"), doc (showString "in"), prt 0 expr])
    MplLanguage.AbsMPL.INFIXR0_EXPR expr1 colon expr2 -> prPrec i 0 (concatD [prt 1 expr1, prt 0 colon, prt 0 expr2])
    MplLanguage.AbsMPL.INFIXL1_EXPR expr1 infixlop expr2 -> prPrec i 1 (concatD [prt 1 expr1, prt 0 infixlop, prt 2 expr2])
    MplLanguage.AbsMPL.INFIXU1_EXPR expr1 infixuop expr2 -> prPrec i 1 (concatD [prt 1 expr1, prt 0 infixuop, prt 2 expr2])
    MplLanguage.AbsMPL.INFIXL2_EXPR expr1 infixlop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 infixlop, prt 3 expr2])
    MplLanguage.AbsMPL.INFIXU2_EXPR expr1 infixuop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 infixuop, prt 3 expr2])
    MplLanguage.AbsMPL.INFIXL3_EXPR expr1 infixlop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 infixlop, prt 4 expr2])
    MplLanguage.AbsMPL.INFIXU3_EXPR expr1 infixuop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 infixuop, prt 4 expr2])
    MplLanguage.AbsMPL.INFIXL4_EXPR expr1 infixlop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 infixlop, prt 5 expr2])
    MplLanguage.AbsMPL.INFIXL5_EXPR expr1 infixlop expr2 -> prPrec i 5 (concatD [prt 5 expr1, prt 0 infixlop, prt 6 expr2])
    MplLanguage.AbsMPL.INFIXU5_EXPR expr1 infixuop expr2 -> prPrec i 5 (concatD [prt 5 expr1, prt 0 infixuop, prt 6 expr2])
    MplLanguage.AbsMPL.INFIXL6_EXPR expr1 infixlop expr2 -> prPrec i 6 (concatD [prt 6 expr1, prt 0 infixlop, prt 7 expr2])
    MplLanguage.AbsMPL.INFIXU6_EXPR expr1 infixuop expr2 -> prPrec i 6 (concatD [prt 6 expr1, prt 0 infixuop, prt 7 expr2])
    MplLanguage.AbsMPL.INFIXR7_EXPR expr1 infixrop expr2 -> prPrec i 7 (concatD [prt 8 expr1, prt 0 infixrop, prt 7 expr2])
    MplLanguage.AbsMPL.INFIXU7_EXPR expr1 infixuop expr2 -> prPrec i 7 (concatD [prt 8 expr1, prt 0 infixuop, prt 7 expr2])
    MplLanguage.AbsMPL.INFIXL8_EXPR expr1 infixlop expr2 -> prPrec i 8 (concatD [prt 8 expr1, prt 0 infixlop, prt 10 expr2])
    MplLanguage.AbsMPL.INFIXU_SECT lbracket1 infixuop rbracket1 lbracket2 expr1 expr2 rbracket2 -> prPrec i 10 (concatD [prt 0 lbracket1, prt 0 infixuop, prt 0 rbracket1, prt 0 lbracket2, prt 0 expr1, doc (showString ","), prt 0 expr2, prt 0 rbracket2])
    MplLanguage.AbsMPL.LIST_EXPR lsquarebracket exprs rsquarebracket -> prPrec i 10 (concatD [prt 0 lsquarebracket, prt 0 exprs, prt 0 rsquarebracket])
    MplLanguage.AbsMPL.VAR_EXPR pident -> prPrec i 10 (concatD [prt 0 pident])
    MplLanguage.AbsMPL.INT_EXPR pinteger -> prPrec i 10 (concatD [prt 0 pinteger])
    MplLanguage.AbsMPL.STRING_EXPR pstring -> prPrec i 10 (concatD [prt 0 pstring])
    MplLanguage.AbsMPL.CHAR_EXPR pchar -> prPrec i 10 (concatD [prt 0 pchar])
    MplLanguage.AbsMPL.DOUBLE_EXPR pdouble -> prPrec i 10 (concatD [prt 0 pdouble])
    MplLanguage.AbsMPL.UNIT_EXPR lbracket rbracket -> prPrec i 10 (concatD [prt 0 lbracket, prt 0 rbracket])
    MplLanguage.AbsMPL.FOLD_EXPR expr foldexprphrases -> prPrec i 10 (concatD [doc (showString "fold"), prt 0 expr, doc (showString "of"), doc (showString "{"), prt 0 foldexprphrases, doc (showString "}")])
    MplLanguage.AbsMPL.UNFOLD_EXPR expr unfoldexprphrases -> prPrec i 10 (concatD [doc (showString "unfold"), prt 0 expr, doc (showString "of"), doc (showString "{"), prt 0 unfoldexprphrases, doc (showString "}")])
    MplLanguage.AbsMPL.CASE_EXPR case_ expr pattexprphrases -> prPrec i 10 (concatD [prt 0 case_, prt 0 expr, doc (showString "of"), doc (showString "{"), prt 0 pattexprphrases, doc (showString "}")])
    MplLanguage.AbsMPL.SWITCH_EXP switchexprphrases -> prPrec i 10 (concatD [doc (showString "switch"), doc (showString "{"), prt 0 switchexprphrases, doc (showString "}")])
    MplLanguage.AbsMPL.DESTRUCTOR_CONSTRUCTOR_ARGS_EXPR uident lbracket exprs rbracket -> prPrec i 10 (concatD [prt 0 uident, prt 0 lbracket, prt 0 exprs, prt 0 rbracket])
    MplLanguage.AbsMPL.DESTRUCTOR_CONSTRUCTOR_NO_ARGS_EXPR uident -> prPrec i 10 (concatD [prt 0 uident])
    MplLanguage.AbsMPL.TUPLE_EXPR lbracket expr tupleexprlists rbracket -> prPrec i 10 (concatD [prt 0 lbracket, prt 0 expr, doc (showString ","), prt 0 tupleexprlists, prt 0 rbracket])
    MplLanguage.AbsMPL.FUN_EXPR pident lbracket exprs rbracket -> prPrec i 10 (concatD [prt 0 pident, prt 0 lbracket, prt 0 exprs, prt 0 rbracket])
    MplLanguage.AbsMPL.RECORD_EXPR lbracket recordexprphrases rbracket -> prPrec i 10 (concatD [prt 0 lbracket, prt 0 recordexprphrases, prt 0 rbracket])
    MplLanguage.AbsMPL.BRACKETED_EXPR lbracket expr rbracket -> prPrec i 10 (concatD [prt 0 lbracket, prt 0 expr, prt 0 rbracket])

instance Print MplLanguage.AbsMPL.InfixUop where
  prt i = \case
    MplLanguage.AbsMPL.InfixUop1 infixuop -> prPrec i 0 (concatD [prt 0 infixuop])
    MplLanguage.AbsMPL.InfixUop2 infixuop -> prPrec i 0 (concatD [prt 0 infixuop])
    MplLanguage.AbsMPL.InfixUop3 infixuop -> prPrec i 0 (concatD [prt 0 infixuop])
    MplLanguage.AbsMPL.InfixUop5 infixuop -> prPrec i 0 (concatD [prt 0 infixuop])
    MplLanguage.AbsMPL.InfixUop6 infixuop -> prPrec i 0 (concatD [prt 0 infixuop])
    MplLanguage.AbsMPL.InfixUop7 infixuop -> prPrec i 0 (concatD [prt 0 infixuop])

instance Print MplLanguage.AbsMPL.UnfoldExprPhrase where
  prt i = \case
    MplLanguage.AbsMPL.UNFOLD_EXPR_PHRASE pattern_ foldexprphrases -> prPrec i 0 (concatD [prt 0 pattern_, doc (showString "of"), doc (showString "{"), prt 0 foldexprphrases, doc (showString "}")])

instance Print [MplLanguage.AbsMPL.UnfoldExprPhrase] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print MplLanguage.AbsMPL.FoldExprPhrase where
  prt i = \case
    MplLanguage.AbsMPL.FOLD_EXPR_PHRASE uident colon patterns expr -> prPrec i 0 (concatD [prt 0 uident, prt 0 colon, prt 0 patterns, doc (showString "->"), prt 0 expr])

instance Print [MplLanguage.AbsMPL.FoldExprPhrase] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print MplLanguage.AbsMPL.LetExprPhrase where
  prt i = \case
    MplLanguage.AbsMPL.LET_EXPR_PHRASE mplstmt -> prPrec i 0 (concatD [prt 0 mplstmt])

instance Print [MplLanguage.AbsMPL.LetExprPhrase] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print MplLanguage.AbsMPL.TupleExprList where
  prt i = \case
    MplLanguage.AbsMPL.TUPLE_EXPR_LIST expr -> prPrec i 0 (concatD [prt 0 expr])

instance Print [MplLanguage.AbsMPL.TupleExprList] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print MplLanguage.AbsMPL.RecordExprPhrase where
  prt i = \case
    MplLanguage.AbsMPL.RECORD_EXPR_PHRASE uident expr -> prPrec i 0 (concatD [prt 0 uident, doc (showString ":="), prt 0 expr])
    MplLanguage.AbsMPL.RECORD_EXPR_HIGHER_ORDER_PHRASE uident pattexprphrase -> prPrec i 0 (concatD [prt 0 uident, doc (showString ":="), prt 0 pattexprphrase])

instance Print [MplLanguage.AbsMPL.RecordExprPhrase] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print MplLanguage.AbsMPL.SwitchExprPhrase where
  prt i = \case
    MplLanguage.AbsMPL.SWITCH_EXPR_PHRASE expr1 expr2 -> prPrec i 0 (concatD [prt 0 expr1, doc (showString "->"), prt 0 expr2])

instance Print [MplLanguage.AbsMPL.SwitchExprPhrase] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print [MplLanguage.AbsMPL.Expr] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print MplLanguage.AbsMPL.PattExprPhrase where
  prt i = \case
    MplLanguage.AbsMPL.PATTERN_TO_EXPR patterns expr -> prPrec i 0 (concatD [prt 0 patterns, doc (showString "->"), prt 0 expr])

instance Print MplLanguage.AbsMPL.Pattern where
  prt i = \case
    MplLanguage.AbsMPL.PATTERN pattern_ -> prPrec i 0 (concatD [prt 0 pattern_])
    MplLanguage.AbsMPL.TYPED_PATTERN pattern_ mpltype -> prPrec i 0 (concatD [prt 0 pattern_, doc (showString "::"), prt 0 mpltype])
    MplLanguage.AbsMPL.LIST_COLON_PATTERN pattern_1 colon pattern_2 -> prPrec i 0 (concatD [prt 1 pattern_1, prt 0 colon, prt 0 pattern_2])
    MplLanguage.AbsMPL.CONSTRUCTOR_PATTERN_ARGS uident lbracket patterns rbracket -> prPrec i 1 (concatD [prt 0 uident, prt 0 lbracket, prt 0 patterns, prt 0 rbracket])
    MplLanguage.AbsMPL.CONSTRUCTOR_PATTERN_NO_ARGS uident -> prPrec i 1 (concatD [prt 0 uident])
    MplLanguage.AbsMPL.UNIT_PATTERN lbracket rbracket -> prPrec i 1 (concatD [prt 0 lbracket, prt 0 rbracket])
    MplLanguage.AbsMPL.RECORD_PATTERN lbracket destructorpatternphrases rbracket -> prPrec i 1 (concatD [prt 0 lbracket, prt 0 destructorpatternphrases, prt 0 rbracket])
    MplLanguage.AbsMPL.LIST_PATTERN lsquarebracket patterns rsquarebracket -> prPrec i 1 (concatD [prt 0 lsquarebracket, prt 0 patterns, prt 0 rsquarebracket])
    MplLanguage.AbsMPL.TUPLE_PATTERN lbracket pattern_ tuplelistpatterns rbracket -> prPrec i 1 (concatD [prt 0 lbracket, prt 0 pattern_, doc (showString ","), prt 0 tuplelistpatterns, prt 0 rbracket])
    MplLanguage.AbsMPL.VAR_PATTERN pident -> prPrec i 1 (concatD [prt 0 pident])
    MplLanguage.AbsMPL.STR_PATTERN pstring -> prPrec i 1 (concatD [prt 0 pstring])
    MplLanguage.AbsMPL.CHAR_PATTERN pchar -> prPrec i 1 (concatD [prt 0 pchar])
    MplLanguage.AbsMPL.INT_PATTERN pinteger -> prPrec i 1 (concatD [prt 0 pinteger])
    MplLanguage.AbsMPL.NULL_PATTERN nullpattern -> prPrec i 1 (concatD [prt 0 nullpattern])
    MplLanguage.AbsMPL.BRACKETED_PATTERN lbracket pattern_ rbracket -> prPrec i 1 (concatD [prt 0 lbracket, prt 0 pattern_, prt 0 rbracket])

instance Print [MplLanguage.AbsMPL.Pattern] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print MplLanguage.AbsMPL.TupleListPattern where
  prt i = \case
    MplLanguage.AbsMPL.TUPLE_LIST_PATTERN pattern_ -> prPrec i 0 (concatD [prt 0 pattern_])

instance Print [MplLanguage.AbsMPL.TupleListPattern] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print MplLanguage.AbsMPL.DestructorPatternPhrase where
  prt i = \case
    MplLanguage.AbsMPL.DESTRUCTOR_PATTERN_PHRASE uident pattern_ -> prPrec i 0 (concatD [prt 0 uident, doc (showString ":="), prt 0 pattern_])

instance Print [MplLanguage.AbsMPL.DestructorPatternPhrase] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print MplLanguage.AbsMPL.FunctionDefn where
  prt i = \case
    MplLanguage.AbsMPL.INTERNAL_TYPED_FUNCTION_DEFN pident mpltype pattexprphrases -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 pident, doc (showString "::"), prt 0 mpltype, doc (showString "="), doc (showString "{"), prt 0 pattexprphrases, doc (showString "}")])
    MplLanguage.AbsMPL.TYPED_FUNCTION_DEFN pident mpltypes mpltype pattexprphrases -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 pident, doc (showString "::"), prt 0 mpltypes, doc (showString "->"), prt 0 mpltype, doc (showString "="), doc (showString "{"), prt 0 pattexprphrases, doc (showString "}")])
    MplLanguage.AbsMPL.FUNCTION_DEFN pident pattexprphrases -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 pident, doc (showString "="), doc (showString "{"), prt 0 pattexprphrases, doc (showString "}")])
    MplLanguage.AbsMPL.TYPED_FUNCTION_DEFN_UINFIX lbracket infixuop rbracket mpltype1 mpltype2 mpltype3 pattexprphrases -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 lbracket, prt 0 infixuop, prt 0 rbracket, doc (showString "::"), prt 0 mpltype1, doc (showString ","), prt 0 mpltype2, doc (showString "->"), prt 0 mpltype3, doc (showString "="), doc (showString "{"), prt 0 pattexprphrases, doc (showString "}")])
    MplLanguage.AbsMPL.FUNCTION_DEFN_UINFIX lbracket infixuop rbracket pattexprphrases -> prPrec i 0 (concatD [doc (showString "fun"), prt 0 lbracket, prt 0 infixuop, prt 0 rbracket, doc (showString "="), doc (showString "{"), prt 0 pattexprphrases, doc (showString "}")])

instance Print [MplLanguage.AbsMPL.PattExprPhrase] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print MplLanguage.AbsMPL.ProcessDefn where
  prt i = \case
    MplLanguage.AbsMPL.TYPED_PROCESS_DEFN pident mpltypes1 mpltypes2 mpltypes3 processphrases -> prPrec i 0 (concatD [doc (showString "proc"), prt 0 pident, doc (showString "::"), prt 0 mpltypes1, doc (showString "|"), prt 0 mpltypes2, doc (showString "=>"), prt 0 mpltypes3, doc (showString "="), doc (showString "{"), prt 0 processphrases, doc (showString "}")])
    MplLanguage.AbsMPL.INTERNAL_TYPED_PROCESS_DEFN pident mpltype processphrases -> prPrec i 0 (concatD [doc (showString "proc"), prt 0 pident, doc (showString "::"), prt 0 mpltype, doc (showString "="), doc (showString "{"), prt 0 processphrases, doc (showString "}")])
    MplLanguage.AbsMPL.PROCESS_DEFN pident processphrases -> prPrec i 0 (concatD [doc (showString "proc"), prt 0 pident, doc (showString "="), doc (showString "{"), prt 0 processphrases, doc (showString "}")])

instance Print MplLanguage.AbsMPL.ProcessPhrase where
  prt i = \case
    MplLanguage.AbsMPL.PROCESS_PHRASE patterns pidents1 pidents2 processcommandsblock -> prPrec i 0 (concatD [prt 0 patterns, doc (showString "|"), prt 0 pidents1, doc (showString "=>"), prt 0 pidents2, doc (showString "->"), prt 0 processcommandsblock])

instance Print [MplLanguage.AbsMPL.ProcessPhrase] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print MplLanguage.AbsMPL.ProcessCommandsBlock where
  prt i = \case
    MplLanguage.AbsMPL.PROCESS_COMMANDS_DO_BLOCK processcommands -> prPrec i 0 (concatD [doc (showString "do"), doc (showString "{"), prt 0 processcommands, doc (showString "}")])
    MplLanguage.AbsMPL.PROCESS_COMMANDS_SINGLE_COMMAND_BLOCK processcommand -> prPrec i 0 (concatD [prt 0 processcommand])

instance Print [MplLanguage.AbsMPL.ProcessCommand] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print MplLanguage.AbsMPL.ProcessCommand where
  prt i = \case
    MplLanguage.AbsMPL.PROCESS_RUN pident lbracket exprs pidents1 pidents2 rbracket -> prPrec i 0 (concatD [prt 0 pident, prt 0 lbracket, prt 0 exprs, doc (showString "|"), prt 0 pidents1, doc (showString "=>"), prt 0 pidents2, prt 0 rbracket])
    MplLanguage.AbsMPL.PROCESS_CLOSE close pident -> prPrec i 0 (concatD [prt 0 close, prt 0 pident])
    MplLanguage.AbsMPL.PROCESS_HALT halt pident -> prPrec i 0 (concatD [prt 0 halt, prt 0 pident])
    MplLanguage.AbsMPL.PROCESS_GET get pattern_ pident -> prPrec i 0 (concatD [prt 0 get, prt 0 pattern_, doc (showString "on"), prt 0 pident])
    MplLanguage.AbsMPL.PROCESS_PUT put expr pident -> prPrec i 0 (concatD [prt 0 put, prt 0 expr, doc (showString "on"), prt 0 pident])
    MplLanguage.AbsMPL.PROCESS_HCASE hcase pident hcasephrases -> prPrec i 0 (concatD [prt 0 hcase, prt 0 pident, doc (showString "of"), doc (showString "{"), prt 0 hcasephrases, doc (showString "}")])
    MplLanguage.AbsMPL.PROCESS_HPUT hput uident pident -> prPrec i 0 (concatD [prt 0 hput, prt 0 uident, doc (showString "on"), prt 0 pident])
    MplLanguage.AbsMPL.PROCESS_ON pident onphrases -> prPrec i 0 (concatD [doc (showString "on"), prt 0 pident, doc (showString "do"), doc (showString "{"), prt 0 onphrases, doc (showString "}")])
    MplLanguage.AbsMPL.PROCESS_SPLIT split pident splitchannels -> prPrec i 0 (concatD [prt 0 split, prt 0 pident, doc (showString "into"), prt 0 splitchannels])
    MplLanguage.AbsMPL.PROCESS_FORK fork pident forkphrases -> prPrec i 0 (concatD [prt 0 fork, prt 0 pident, doc (showString "as"), doc (showString "{"), prt 0 forkphrases, doc (showString "}")])
    MplLanguage.AbsMPL.PROCESS_ID pident1 chid pident2 -> prPrec i 0 (concatD [prt 0 pident1, prt 0 chid, prt 0 pident2])
    MplLanguage.AbsMPL.PROCESS_NEG pident1 chid pident2 -> prPrec i 0 (concatD [prt 0 pident1, prt 0 chid, doc (showString "neg"), prt 0 pident2])
    MplLanguage.AbsMPL.PROCESS_RACE racephrases -> prPrec i 0 (concatD [doc (showString "race"), doc (showString "{"), prt 0 racephrases, doc (showString "}")])
    MplLanguage.AbsMPL.PROCESS_PLUG plugphrases -> prPrec i 0 (concatD [doc (showString "plug"), doc (showString "{"), prt 0 plugphrases, doc (showString "}")])
    MplLanguage.AbsMPL.PROCESS_CASE case_ expr processcasephrases -> prPrec i 0 (concatD [prt 0 case_, prt 0 expr, doc (showString "of"), doc (showString "{"), prt 0 processcasephrases, doc (showString "}")])
    MplLanguage.AbsMPL.PROCESS_IF expr processcommandsblock1 processcommandsblock2 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 expr, doc (showString "then"), prt 0 processcommandsblock1, doc (showString "else"), prt 0 processcommandsblock2])
    MplLanguage.AbsMPL.PROCESS_SWITCH processswitchphrases -> prPrec i 0 (concatD [doc (showString "switch"), doc (showString "{"), prt 0 processswitchphrases, doc (showString "}")])

instance Print MplLanguage.AbsMPL.OnPhrase where
  prt i = \case
    MplLanguage.AbsMPL.ON_PUT put expr -> prPrec i 0 (concatD [prt 0 put, prt 0 expr])
    MplLanguage.AbsMPL.ON_GET get pattern_ -> prPrec i 0 (concatD [prt 0 get, prt 0 pattern_])
    MplLanguage.AbsMPL.ON_HPUT hput uident -> prPrec i 0 (concatD [prt 0 hput, prt 0 uident])
    MplLanguage.AbsMPL.ON_HCASE hcase hcasephrases -> prPrec i 0 (concatD [prt 0 hcase, doc (showString "of"), doc (showString "{"), prt 0 hcasephrases, doc (showString "}")])
    MplLanguage.AbsMPL.ON_FORK fork forkphrases -> prPrec i 0 (concatD [prt 0 fork, doc (showString "as"), doc (showString "{"), prt 0 forkphrases, doc (showString "}")])
    MplLanguage.AbsMPL.ON_SPLIT split splitchannels -> prPrec i 0 (concatD [prt 0 split, doc (showString "into"), prt 0 splitchannels])
    MplLanguage.AbsMPL.ON_CLOSE close -> prPrec i 0 (concatD [prt 0 close])
    MplLanguage.AbsMPL.ON_HALT halt -> prPrec i 0 (concatD [prt 0 halt])

instance Print [MplLanguage.AbsMPL.OnPhrase] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print MplLanguage.AbsMPL.HCasePhrase where
  prt i = \case
    MplLanguage.AbsMPL.HCASE_PHRASE uident processcommandsblock -> prPrec i 0 (concatD [prt 0 uident, doc (showString "->"), prt 0 processcommandsblock])

instance Print [MplLanguage.AbsMPL.HCasePhrase] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print MplLanguage.AbsMPL.SplitChannel where
  prt i = \case
    MplLanguage.AbsMPL.SPLIT_CHANNEL pident -> prPrec i 0 (concatD [prt 0 pident])

instance Print [MplLanguage.AbsMPL.SplitChannel] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print MplLanguage.AbsMPL.ForkPhrase where
  prt i = \case
    MplLanguage.AbsMPL.FORK_PHRASE pident processcommandsblock -> prPrec i 0 (concatD [prt 0 pident, doc (showString "->"), prt 0 processcommandsblock])
    MplLanguage.AbsMPL.FORK_WITH_PHRASE pident forkchannels processcommandsblock -> prPrec i 0 (concatD [prt 0 pident, doc (showString "with"), prt 0 forkchannels, doc (showString "->"), prt 0 processcommandsblock])

instance Print [MplLanguage.AbsMPL.ForkPhrase] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print MplLanguage.AbsMPL.ForkChannel where
  prt i = \case
    MplLanguage.AbsMPL.FORK_CHANNEL pident -> prPrec i 0 (concatD [prt 0 pident])

instance Print [MplLanguage.AbsMPL.ForkChannel] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print MplLanguage.AbsMPL.RacePhrase where
  prt i = \case
    MplLanguage.AbsMPL.RACE_PHRASE pident processcommandsblock -> prPrec i 0 (concatD [prt 0 pident, doc (showString "->"), prt 0 processcommandsblock])

instance Print [MplLanguage.AbsMPL.RacePhrase] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print MplLanguage.AbsMPL.PlugPhrase where
  prt i = \case
    MplLanguage.AbsMPL.PLUG_PHRASE processcommandsblock -> prPrec i 0 (concatD [prt 0 processcommandsblock])
    MplLanguage.AbsMPL.PLUG_PHRASE_AS pidents1 pidents2 processcommandsblock -> prPrec i 0 (concatD [prt 0 pidents1, doc (showString "=>"), prt 0 pidents2, doc (showString "->"), prt 0 processcommandsblock])

instance Print [MplLanguage.AbsMPL.PlugPhrase] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print MplLanguage.AbsMPL.ProcessCasePhrase where
  prt i = \case
    MplLanguage.AbsMPL.PROCESS_CASE_PHRASE pattern_ processcommandsblock -> prPrec i 0 (concatD [prt 0 pattern_, doc (showString "->"), prt 0 processcommandsblock])

instance Print [MplLanguage.AbsMPL.ProcessCasePhrase] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print MplLanguage.AbsMPL.ProcessSwitchPhrase where
  prt i = \case
    MplLanguage.AbsMPL.PROCESS_SWITCH_PHRASE expr processcommandsblock -> prPrec i 0 (concatD [prt 0 expr, doc (showString "->"), prt 0 processcommandsblock])

instance Print [MplLanguage.AbsMPL.ProcessSwitchPhrase] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]
