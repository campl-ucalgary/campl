-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Lexer definition for use with Alex 3
{
{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -w #-}

{-# LANGUAGE PatternSynonyms #-}

module MplLanguage.LexMPL where

import Prelude

import qualified Data.Bits
import Data.Char     (ord)
import Data.Function (on)
import Data.Word     (Word8)
}

-- Predefined character classes

$c = [A-Z\192-\221] # [\215]  -- capital isolatin1 letter (215 = \times) FIXME
$s = [a-z\222-\255] # [\247]  -- small   isolatin1 letter (247 = \div  ) FIXME
$l = [$c $s]         -- letter
$d = [0-9]           -- digit
$i = [$l $d _ ']     -- identifier character
$u = [. \n]          -- universal: any character

-- Symbols and non-identifier-like reserved words

@rsyms = \, | \{ | \} | \; | \| | \- \> | \= | \: \: | \= \> | \. | \: \=

:-

-- Line comment "--"
"--" [.]* ;

-- Block comment "{-" "-}"
\{ \- [$u # \-]* \- ([$u # [\- \}]] [$u # \-]* \- | \-)* \} ;

-- Whitespace (skipped)
$white+ ;

-- Symbols
@rsyms
    { tok (eitherResIdent TV) }

-- token PInteger
(\- $d | $d)$d *
    { tok (eitherResIdent T_PInteger) }

-- token PDouble
$d + \. $d + (e \- ? $d +)?
    { tok (eitherResIdent T_PDouble) }

-- token PChar
\' ([$u # [\' \\]] | \\ [\' \\ f n r t]) \'
    { tok (eitherResIdent T_PChar) }

-- token PString
\" ([$u # [\" \\]] | \\ [\" \\ f n r t]) * \"
    { tok (eitherResIdent T_PString) }

-- token Par
\( \+ \)
    { tok (eitherResIdent T_Par) }

-- token Tensor
\( \* \)
    { tok (eitherResIdent T_Tensor) }

-- token LBracket
\(
    { tok (eitherResIdent T_LBracket) }

-- token RBracket
\)
    { tok (eitherResIdent T_RBracket) }

-- token LSquareBracket
\[
    { tok (eitherResIdent T_LSquareBracket) }

-- token RSquareBracket
\]
    { tok (eitherResIdent T_RSquareBracket) }

-- token NullPattern
\_
    { tok (eitherResIdent T_NullPattern) }

-- token Colon
\:
    { tok (eitherResIdent T_Colon) }

-- token Infixl3op
\= \= | \/ \= | \< | \> | \< \= | \> \=
    { tok (eitherResIdent T_Infixl3op) }

-- token Infixl5op
[\+ \-]
    { tok (eitherResIdent T_Infixl5op) }

-- token Infixl6op
[\* \/]
    { tok (eitherResIdent T_Infixl6op) }

-- token ChId
\| \= \|
    { tok (eitherResIdent T_ChId) }

-- token InfixU1op
\| ([\! \# \$ \% \& \* \+ \- \/ \: \< \> \? \@ \^ \_ \| \~][\! \# \$ \% \& \* \+ \- \/ \: \< \= \> \? \@ \^ \_ \| \~]* | \= [\! \# \$ \% \& \* \+ \- \/ \: \< \= \? \@ \^ \_ \| \~][\! \# \$ \% \& \* \+ \- \/ \: \< \= \> \? \@ \^ \_ \| \~]* | \= \> [\! \# \$ \% \& \* \+ \- \/ \: \< \= \> \? \@ \^ \_ \| \~]+)
    { tok (eitherResIdent T_InfixU1op) }

-- token InfixU2op
\& [\! \# \$ \% \& \* \+ \- \/ \: \< \= \> \? \@ \^ \_ \| \~]*
    { tok (eitherResIdent T_InfixU2op) }

-- token InfixU3op
[\! \< \>][\! \# \$ \% \& \* \+ \- \/ \: \< \= \> \? \@ \^ \_ \| \~]*
    { tok (eitherResIdent T_InfixU3op) }

-- token InfixU5op
[\+ \-][\! \# \$ \% \& \* \+ \- \/ \: \< \= \> \? \@ \^ \_ \| \~]*
    { tok (eitherResIdent T_InfixU5op) }

-- token InfixU6op
[\% \* \/][\! \# \$ \% \& \* \+ \- \/ \: \< \= \> \? \@ \^ \_ \| \~]*
    { tok (eitherResIdent T_InfixU6op) }

-- token InfixU7op
\^ [\! \# \$ \% \& \* \+ \- \/ \: \< \= \> \? \@ \^ \_ \| \~]*
    { tok (eitherResIdent T_InfixU7op) }

-- token Infixl1op
\| \|
    { tok (eitherResIdent T_Infixl1op) }

-- token Infixl2op
\& \&
    { tok (eitherResIdent T_Infixl2op) }

-- token Infixl4op
\+ \+
    { tok (eitherResIdent T_Infixl4op) }

-- token Infixr7op
\^
    { tok (eitherResIdent T_Infixr7op) }

-- token Infixl8op
\! \!
    { tok (eitherResIdent T_Infixl8op) }

-- token Close
c l o s e
    { tok (eitherResIdent T_Close) }

-- token Halt
h a l t
    { tok (eitherResIdent T_Halt) }

-- token Get
g e t
    { tok (eitherResIdent T_Get) }

-- token Put
p u t
    { tok (eitherResIdent T_Put) }

-- token HCase
h c a s e
    { tok (eitherResIdent T_HCase) }

-- token HPut
h p u t
    { tok (eitherResIdent T_HPut) }

-- token Split
s p l i t
    { tok (eitherResIdent T_Split) }

-- token Fork
f o r k
    { tok (eitherResIdent T_Fork) }

-- token Case
c a s e
    { tok (eitherResIdent T_Case) }

-- token UIdent
$c (\_ | ($d | $l)) *
    { tok (eitherResIdent T_UIdent) }

-- token PIdent
(\_ | $l)([\' \_]| ($d | $l)) *
    { tok (eitherResIdent T_PIdent) }

-- token UPIdent
$l ([\' \_]| ($d | $l)) *
    { tok (eitherResIdent T_UPIdent) }

-- Keywords and Ident
$l $i*
    { tok (eitherResIdent TV) }

{
-- | Create a token with position.
tok :: (String -> Tok) -> (Posn -> String -> Token)
tok f p = PT p . f

-- | Token without position.
data Tok
  = TK {-# UNPACK #-} !TokSymbol  -- ^ Reserved word or symbol.
  | TL !String                    -- ^ String literal.
  | TI !String                    -- ^ Integer literal.
  | TV !String                    -- ^ Identifier.
  | TD !String                    -- ^ Float literal.
  | TC !String                    -- ^ Character literal.
  | T_PInteger !String
  | T_PDouble !String
  | T_PChar !String
  | T_PString !String
  | T_Par !String
  | T_Tensor !String
  | T_LBracket !String
  | T_RBracket !String
  | T_LSquareBracket !String
  | T_RSquareBracket !String
  | T_NullPattern !String
  | T_Colon !String
  | T_Infixl3op !String
  | T_Infixl5op !String
  | T_Infixl6op !String
  | T_ChId !String
  | T_InfixU1op !String
  | T_InfixU2op !String
  | T_InfixU3op !String
  | T_InfixU5op !String
  | T_InfixU6op !String
  | T_InfixU7op !String
  | T_Infixl1op !String
  | T_Infixl2op !String
  | T_Infixl4op !String
  | T_Infixr7op !String
  | T_Infixl8op !String
  | T_Close !String
  | T_Halt !String
  | T_Get !String
  | T_Put !String
  | T_HCase !String
  | T_HPut !String
  | T_Split !String
  | T_Fork !String
  | T_Case !String
  | T_UIdent !String
  | T_PIdent !String
  | T_UPIdent !String
  deriving (Eq, Show, Ord)

-- | Smart constructor for 'Tok' for the sake of backwards compatibility.
pattern TS :: String -> Int -> Tok
pattern TS t i = TK (TokSymbol t i)

-- | Keyword or symbol tokens have a unique ID.
data TokSymbol = TokSymbol
  { tsText :: String
      -- ^ Keyword or symbol text.
  , tsID   :: !Int
      -- ^ Unique ID.
  } deriving (Show)

-- | Keyword/symbol equality is determined by the unique ID.
instance Eq  TokSymbol where (==)    = (==)    `on` tsID

-- | Keyword/symbol ordering is determined by the unique ID.
instance Ord TokSymbol where compare = compare `on` tsID

-- | Token with position.
data Token
  = PT  Posn Tok
  | Err Posn
  deriving (Eq, Show, Ord)

-- | Pretty print a position.
printPosn :: Posn -> String
printPosn (Pn _ l c) = "line " ++ show l ++ ", column " ++ show c

-- | Pretty print the position of the first token in the list.
tokenPos :: [Token] -> String
tokenPos (t:_) = printPosn (tokenPosn t)
tokenPos []    = "end of file"

-- | Get the position of a token.
tokenPosn :: Token -> Posn
tokenPosn (PT p _) = p
tokenPosn (Err p)  = p

-- | Get line and column of a token.
tokenLineCol :: Token -> (Int, Int)
tokenLineCol = posLineCol . tokenPosn

-- | Get line and column of a position.
posLineCol :: Posn -> (Int, Int)
posLineCol (Pn _ l c) = (l,c)

-- | Convert a token into "position token" form.
mkPosToken :: Token -> ((Int, Int), String)
mkPosToken t = (tokenLineCol t, tokenText t)

-- | Convert a token to its text.
tokenText :: Token -> String
tokenText t = case t of
  PT _ (TS s _) -> s
  PT _ (TL s)   -> show s
  PT _ (TI s)   -> s
  PT _ (TV s)   -> s
  PT _ (TD s)   -> s
  PT _ (TC s)   -> s
  Err _         -> "#error"
  PT _ (T_PInteger s) -> s
  PT _ (T_PDouble s) -> s
  PT _ (T_PChar s) -> s
  PT _ (T_PString s) -> s
  PT _ (T_Par s) -> s
  PT _ (T_Tensor s) -> s
  PT _ (T_LBracket s) -> s
  PT _ (T_RBracket s) -> s
  PT _ (T_LSquareBracket s) -> s
  PT _ (T_RSquareBracket s) -> s
  PT _ (T_NullPattern s) -> s
  PT _ (T_Colon s) -> s
  PT _ (T_Infixl3op s) -> s
  PT _ (T_Infixl5op s) -> s
  PT _ (T_Infixl6op s) -> s
  PT _ (T_ChId s) -> s
  PT _ (T_InfixU1op s) -> s
  PT _ (T_InfixU2op s) -> s
  PT _ (T_InfixU3op s) -> s
  PT _ (T_InfixU5op s) -> s
  PT _ (T_InfixU6op s) -> s
  PT _ (T_InfixU7op s) -> s
  PT _ (T_Infixl1op s) -> s
  PT _ (T_Infixl2op s) -> s
  PT _ (T_Infixl4op s) -> s
  PT _ (T_Infixr7op s) -> s
  PT _ (T_Infixl8op s) -> s
  PT _ (T_Close s) -> s
  PT _ (T_Halt s) -> s
  PT _ (T_Get s) -> s
  PT _ (T_Put s) -> s
  PT _ (T_HCase s) -> s
  PT _ (T_HPut s) -> s
  PT _ (T_Split s) -> s
  PT _ (T_Fork s) -> s
  PT _ (T_Case s) -> s
  PT _ (T_UIdent s) -> s
  PT _ (T_PIdent s) -> s
  PT _ (T_UPIdent s) -> s

-- | Convert a token to a string.
prToken :: Token -> String
prToken t = tokenText t

-- | Finite map from text to token organized as binary search tree.
data BTree
  = N -- ^ Nil (leaf).
  | B String Tok BTree BTree
      -- ^ Binary node.
  deriving (Show)

-- | Convert potential keyword into token or use fallback conversion.
eitherResIdent :: (String -> Tok) -> String -> Tok
eitherResIdent tv s = treeFind resWords
  where
  treeFind N = tv s
  treeFind (B a t left right) =
    case compare s a of
      LT -> treeFind left
      GT -> treeFind right
      EQ -> t

-- | The keywords and symbols of the language organized as binary search tree.
resWords :: BTree
resWords =
  b "in" 20
    (b "as" 10
       (b ":=" 5
          (b "." 3 (b "->" 2 (b "," 1 N N) N) (b "::" 4 N N))
          (b "=>" 8 (b "=" 7 (b ";" 6 N N) N) (b "and" 9 N N)))
       (b "do" 15
          (b "data" 13
             (b "coprotocol" 12 (b "codata" 11 N N) N) (b "defn" 14 N N))
          (b "fun" 18 (b "fold" 17 (b "else" 16 N N) N) (b "if" 19 N N))))
    (b "race" 30
       (b "on" 25
          (b "neg" 23 (b "let" 22 (b "into" 21 N N) N) (b "of" 24 N N))
          (b "proc" 28
             (b "potato" 27 (b "plug" 26 N N) N) (b "protocol" 29 N N)))
       (b "with" 35
          (b "unfold" 33
             (b "then" 32 (b "switch" 31 N N) N) (b "where" 34 N N))
          (b "|" 37 (b "{" 36 N N) (b "}" 38 N N))))
  where
  b s n = B bs (TS bs n)
    where
    bs = s

-- | Unquote string literal.
unescapeInitTail :: String -> String
unescapeInitTail = id . unesc . tail . id
  where
  unesc s = case s of
    '\\':c:cs | elem c ['\"', '\\', '\''] -> c : unesc cs
    '\\':'n':cs  -> '\n' : unesc cs
    '\\':'t':cs  -> '\t' : unesc cs
    '\\':'r':cs  -> '\r' : unesc cs
    '\\':'f':cs  -> '\f' : unesc cs
    '"':[]       -> []
    c:cs         -> c : unesc cs
    _            -> []

-------------------------------------------------------------------
-- Alex wrapper code.
-- A modified "posn" wrapper.
-------------------------------------------------------------------

data Posn = Pn !Int !Int !Int
  deriving (Eq, Show, Ord)

alexStartPos :: Posn
alexStartPos = Pn 0 1 1

alexMove :: Posn -> Char -> Posn
alexMove (Pn a l c) '\t' = Pn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (Pn a l c) '\n' = Pn (a+1) (l+1)   1
alexMove (Pn a l c) _    = Pn (a+1)  l     (c+1)

type Byte = Word8

type AlexInput = (Posn,     -- current position,
                  Char,     -- previous char
                  [Byte],   -- pending bytes on the current char
                  String)   -- current input string

tokens :: String -> [Token]
tokens str = go (alexStartPos, '\n', [], str)
    where
      go :: AlexInput -> [Token]
      go inp@(pos, _, _, str) =
               case alexScan inp 0 of
                AlexEOF                   -> []
                AlexError (pos, _, _, _)  -> [Err pos]
                AlexSkip  inp' len        -> go inp'
                AlexToken inp' len act    -> act pos (take len str) : (go inp')

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p, c, (b:bs), s) = Just (b, (p, c, bs, s))
alexGetByte (p, _, [], s) =
  case s of
    []  -> Nothing
    (c:s) ->
             let p'     = alexMove p c
                 (b:bs) = utf8Encode c
              in p' `seq` Just (b, (p', c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p, c, bs, s) = c

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
  where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
}
