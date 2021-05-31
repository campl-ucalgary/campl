{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
module MplParse.MplParse where

import Optics
import Optics.State.Operators

import MplParse.Util

import MplParse.Stack
import qualified Text.Parsec as P
import Text.Parsec ((<?>))
import Text.Parsec ((<|>))
import qualified Text.Parsec.Token
import qualified Text.Parsec.Language

import Data.Char
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Control.Applicative hiding ((<|>))

import Control.Monad

import Text.RawString.QQ

import Data.Set (Set)
import qualified Data.Set as Set 

import Debug.Trace

-- * Parsing primitives for removing junk
pComment :: MplParser ()
pComment = P.choice
    [ void $ P.try (P.string "--") *> P.manyTill P.anyChar (P.try P.endOfLine)
    , void $ P.try (P.string "{-") *> P.manyTill P.anyChar (P.try (P.string "-}"))
    ] *> pSpaces

-- | pSpaces. Remove all space characters. Note that we do not use @Text.Parsec.spaces@ since
-- that will include "space" in the first set of tokens which makes error messages misleading.
pSpaces :: MplParser ()
pSpaces = void (P.many (P.satisfy isSpace))

pJunk :: MplParser ()
pJunk = pSpaces <|> pComment 

pLexeme :: MplParser a -> MplParser (Spanned a)
pLexeme pa = (go <$> getPos <*> pa <*> getPos) <* pJunk
  where
    -- Note: we need to @pred@ the ending span since this will
    -- finish one past the end of the string
    go p0 a p1 = Spanned (Span p0 (p1 & col %~ pred)) a

-- | getPos
getPos :: MplParser Pos
getPos = P.getPosition >>= \pos -> return $ 
    Pos { _row = P.sourceLine pos 
        , _col = P.sourceColumn pos 
        }

-- * Wrappers for Parsec functions.
{-| pString. This parses a terminal (i.e., a string in this case) -}
pString :: String -> MplParser (Spanned String)
pString str = pLexeme $ P.try $ indentTerminal (P.string str <?> str)


{-| pChar. Completely similar to 'pString'
-}
pChar :: Char -> MplParser (Spanned Char)
pChar c = pLexeme $ P.try $ indentTerminal (P.char c <?> [c])

-- * Specializing indentation operators.

{-| pIndentBlock. This parses an indentation block -- it simply specializes 'indentBlock' -}
pIndentBlock ::
    -- | Parser for the layout keyword (e.g. @do@ in Haskell)
    MplParser (Spanned String) -> 
    -- | Parser for one line of the indented block
    MplParser a -> 
    -- | Returns: (Parsed layout keyword, list of parses in the indented block)
    MplParser (Spanned String, [a])
pIndentBlock playoutword pidentline = 
    indentBlock playoutword 
        (pChar '{' <?> "") (pChar '}') 
        pidentline 
        (pChar ';' <?> "")

-- * Parsers for parsing quoted strings and characters
pQuotedString :: MplParser String
pQuotedString = 
    P.try 
    $ indentGt 
    $ indentTerminal (Text.Parsec.Token.stringLiteral 
        (Text.Parsec.Token.makeTokenParser Text.Parsec.Language.haskellDef) 
        <?> "quoted string"
        )

pQuotedChar :: MplParser Char
pQuotedChar = 
    P.try 
    $ indentGt 
    $ indentTerminal (
        Text.Parsec.Token.charLiteral 
        (Text.Parsec.Token.makeTokenParser Text.Parsec.Language.haskellDef) 
        <?> "quoted char"
        )

-- * Keywords and convenient wrappers specific to MPL
{-| pTopLevelKeyword. This is used for parsing a keyword (calls `pLexeme`).
-}
pTopLevelKeyword :: String -> MplParser (Spanned String)
pTopLevelKeyword str = indentTerminal $ pLexeme (P.try (P.string str) <?> "keyword")

{-| pUIdent. This is used for parsing identifiers starting with an upper case letter. 
Moreover, this automatically sets the indentation to be strictly greater than the parent
as is essentially always the case in the grammar.
-} 
pUIdent :: MplParser (Spanned String)
pUIdent = pLexeme $ P.try $ indentGt $ indentTerminal (go <?> "uppercase identifier")
  where
    go = (:) <$> P.upper <*> P.many P.alphaNum 


{-| We define all valid symbols as given in this string. 
This copies the Haskell report:
https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4
See 2.2 @ascSymbol@
NOTE: Strictly speaking, we should include unicode symbols as well, so this is TODO.
-}
mPLSYMBOLS :: Set Char
mPLSYMBOLS = Set.fromList [r|!#$%&*+./<=>?@\^|-~:|]

mPLRESERVEDKEYWORDS :: Set String
mPLRESERVEDKEYWORDS = Set.fromList 
    [ "data"
    , "codata"
    , "protocol"
    , "coprotocol"
    , "let"
    ]

mPLRESERVEDSYMBOLS :: Set String
mPLRESERVEDSYMBOLS = Set.fromList 
    [ "::"
    , "->"
    , "=>"
    , "|"
    , ":"
    ]

pPhraseSymbol :: MplParser (Spanned String)
pPhraseSymbol = pLexeme 
    $ P.try 
    $ indentGt 
    $ indentTerminal (go <?> "symbol")
  where
    go = (:) <$> P.char ':' <*> P.many (P.satisfy (`Set.member`mPLSYMBOLS)) >>= \res -> 
        if res `Set.notMember` mPLRESERVEDSYMBOLS then pure res else P.unexpected "reserved function symbol"

{-| pPhraseAsFun. This is used for parsing phrases (constructors and destructors) in data and codata -}
pPhrase :: MplParser (Spanned String)
pPhrase = pUIdent <|> pBracketedPhraseSymbol <?> "constructor/destructor"
  where
    -- duplicated code from 'pPhraseSymbol'
    pBracketedPhraseSymbol = pLexeme $ P.try $ indentGt $ indentTerminal $ 
            P.between 
                (P.char '(') 
                (P.char ')') 
                ((:) <$> P.char ':' <*> P.many (P.satisfy (`Set.member` mPLSYMBOLS))) 
                    >>= \res -> if res `Set.notMember` mPLRESERVEDSYMBOLS 
                            then pure res 
                            else P.unexpected "reserved symbol"

-- | pUIdent. This is used for parsing identifiers starting with 
-- a lower case letter.
-- NOTE: this is essentially duplicated code from 'pLIdent'.
pLIdentTopLevel :: MplParser (Spanned String)
pLIdentTopLevel = pLexeme $ P.try $ indentTerminal (go <?> "lowercase identifier")
  where 
    go = P.many1 P.alphaNum >>= \res -> 
        if res `Set.notMember` mPLRESERVEDKEYWORDS
            then pure res
            else P.unexpected "reserved keyword"

-- | pUIdent. This is used for parsing identifiers starting with 
-- a lower case letter.
pLIdent :: MplParser (Spanned String)
pLIdent = pLexeme $ P.try $ indentGt $ indentTerminal (go <?> "lowercase identifier")
  where 
    go = P.many1 P.alphaNum >>= \res -> 
        if res `Set.notMember` mPLRESERVEDKEYWORDS
            then pure res
            else P.unexpected "reserved keyword"

{-| pSymbol. Symbols almost always are greater than or equal to their parent, so we make 
this convenient wrapper around a string.
-}
pSymbol :: String -> MplParser (Spanned String)
pSymbol str = pLexeme $ P.try $ indentGt $ indentTerminal (P.string str <?> str)

-- * Parsers for MPL types
{-| A /non higher order type/  (non function type) parser. 
NOTE: This assumes that everything is a /variable/ (i.e., not a user defined type)
Indeed, this should be resolved later.
-}
-- pNonFunctionType :: MplParser (MplType MplNewParsed) 
pNonFunctionType = pTy0
  where
    {-| Parses par first (weaker precdence) -}
    -- pTy0 :: MplParser (MplType MplNewParsed)
    pTy0 = P.chainl1 pTy1 pPar
      where
        -- pPar :: MplParser (MplType MplNewParsed -> MplType MplNewParsed -> MplType MplNewParsed)
        -- pPar = pSymbol "(+)" >>= \par -> pure $ \l r -> _TypeParF #  (par ^. spannedSpan, l, r)
        pPar = pSymbol "(+)" >> return (const . const ())
    
    {-| Parses a tensor next (higher precdence) -}
    -- pTy1 :: MplParser (MplType MplNewParsed)
    pTy1 = P.chainl1 pTyAtom pTensor
      where
        -- pTensor :: MplParser (MplType MplNewParsed -> MplType MplNewParsed -> MplType MplNewParsed)
        -- pTensor = pSymbol "(*)" >>= \par -> pure $ \l r -> _TypeTensorF #  (par ^. spannedSpan, l, r)
        pTensor = pSymbol "(*)" >> return (const . const ())

    {-| Parses a type atom -}
    -- pTyAtom :: MplParser (MplType MplNewParsed)
    pTyAtom = 
        pTyVar
        <|> pTySeq
        <|> pTyConc

    {-| Parses a plain type variable e.g. @A@, @Banana@, etc. -}
    -- pTyVar :: MplParser (MplType MplNewParsed)
    -- pTyVar = TypeVar () <$> pUIdent
    pTyVar = pUIdent >> return ()

    {-| Parses a sequential type e.g. @A()@, @Banana(Cat)@, etc. -}
    -- pTySeq :: MplParser (MplType MplNewParsed)
    pTySeq = pUIdent 
        >> P.between 
            (indentGt $ pChar '(') 
            (indentGt $ pChar ')') 
            (pNonFunctionType `P.sepBy` indentGt (pChar ','))
        >> return ()
    {-
    pTySeq = TypeSeqVarWithArgs ()
        <$> pUIdent 
        <*> P.between 
            (indentGt $ pChar '(') 
            (indentGt $ pChar ')') 
            (pNonFunctionType `P.sepBy` indentGt (pChar ','))
    -}

    {-| Parses a concurrent type e.g. @A( | )@, @Banana(A,B| C,D)@, etc. -}
    -- pTyConc :: MplParser (MplType MplNewParsed)
    pTyConc = 
        pUIdent 
        >> P.between 
            (indentGt $ pChar '(') 
            (indentGt $ pChar ')') 
            ( (,) 
                <$> (pNonFunctionType `P.sepBy` indentGt (pChar ',') <* pSymbol "|")
                <*> pNonFunctionType `P.sepBy` indentGt (pChar ',')
            )
        >> return ()
    {-
    pTyConc = TypeConcVarWithArgs ()
        <$> pUIdent 
        <*> P.between 
            (indentGt $ pChar '(') 
            (indentGt $ pChar ')') 
            ( (,) 
                <$> (pNonFunctionType `P.sepBy` indentGt (pChar ',') <* pSymbol "|")
                <*> pNonFunctionType `P.sepBy` indentGt (pChar ',')
            )
    -}

{-| A /type seq arg/ is something like
@ 
    (A,B,C)
@ i.e., a list of arguments of type variables
 -}
pTypeSeqArgsDec :: MplParser [Spanned String]
pTypeSeqArgsDec = (P.between (indentGt $ pChar '(') (indentGt $ pChar ')') $ 
        pUIdent `P.sepBy` indentGt (pChar ','))
        <|> return []

{-| A /type clause/ and a /type phrase/ for /data/ are defined as follows in MPL
@
data 
    Clause1(A,B,C) -> Z
        Phrase11 :: A -> Z
        Phrase12 :: A -> Z
    and
    Clause2(A,B,C) -> X
        Phrase21 :: A -> X
        Phrase22 :: A -> X
    ..
@
-}
-- pMplDataClauseSpine :: MplParser (MplTypeClauseSpine MplNewParsed (SeqObjTag DataDefnTag))
pMplDataClauseSpine = do
    (_dataspan, clauses) <- pIndentBlock (pTopLevelKeyword "data") $ pMplDataClause 
    if null clauses
        then P.unexpected "empty @data@ block"
        -- else return $ UMplTypeClauseSpine $ NE.fromList clauses
        else return ()
  where
    -- pMplDataClause :: MplParser (MplTypeClause MplNewParsed (SeqObjTag DataDefnTag))
    pMplDataClause = do
        clausename <- pUIdent
        typeargs <-pTypeSeqArgsDec
        pSymbol "->"
        statevar <- pUIdent <?> "state variable"
        ~(_equalspan, phrases) <- pIndentBlock (pSymbol "=") pMplDataPhrase

        return ()
        {-
        return $ UMplTypeClause 
            clausename
            typeargs
            statevar
            phrases
        -}

    -- pMplDataPhrase :: MplParser (MplTypePhrase MplNewParsed (SeqObjTag DataDefnTag))
    pMplDataPhrase = do
            cts <- pPhrase
            pSymbol "::"
            args <- P.many pNonFunctionType
            pSymbol "->"
            statevar' <- pUIdent <?> "state variable"
            return $ ()
            -- return $ MplTypePhrase cts args statevar' ()

-- * Parsers for sequential expressions.
parseExprChain = undefined
