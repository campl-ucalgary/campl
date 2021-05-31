{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
import Data.Either

import Text.Parsec ((<|>))
import qualified Text.Parsec as P

import qualified Data.Bifunctor as Bi
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Debug.Trace


type Parser = P.Parsec String ()  

data Infixity 
    = InfixR
    | InfixL
    | InfixE
  deriving (Show, Eq)

data Expr
    = Op (Infixity, Int) Expr Expr
    | Var String
 deriving Show

explicitBracket :: Expr -> String
explicitBracket (Op (assoc, prec) l r) = concat 
    [ "("
    , explicitBracket l 
    , "`"
    , show assoc 
    , show prec
    , "`"
    , explicitBracket r
    , ")"
    ]
explicitBracket (Var str) = str 

newtype TokOp = TokOp (Infixity, Int)
  deriving Show

data TokExprs 
    = TokExprs :// [(TokOp, TokExprs)]
    | TokPrim Expr
  deriving Show


$(makeBaseFunctor ''Expr)

minOp :: TokOp 
minOp = TokOp (InfixE, -1)

resolveInfix :: TokExprs -> Either String Expr
resolveInfix = go 
  where 
    -- _op0@(TokOp (_assoc0, _prec0))
    go :: TokExprs -> Either String Expr
    go (TokPrim expr) = return expr
    -- go (exprs :// []) = resolveInfix exprs
    go (exprs :// rst) = do
        expr <- resolveInfix exprs
        fmap fst $ go' minOp expr rst

    -- The caller is responsible for combining expressions with operators
    {- Here's the idea of this algorithm
     - Let's say we have an expression like: 
     -      a `f` b `g` c ...
     - Some cases.
     -      When we need to associate left should be obvious: (a `f` b) `g` c ...
     -      Associating right is the issue however... We can't just do: a `f` (b `g` c)...
     -          since the right associativity may need to be placed lower.
     -          Hence, we need to recurse to find the next right expression i.e., essentially a "c'".
    -}
    go' :: TokOp -> Expr -> [(TokOp, TokExprs)] -> Either String (Expr, [(TokOp, TokExprs)])
    go' _ expr [] = return (expr,[])
    -- go' op0@(TokOp (assoc0, prec0)) l ((op1@(TokOp (assoc1, prec1)), r):rst) = go r >>= \r -> if
    go' op0@(TokOp (assoc0, prec0)) l ((op1@(TokOp (assoc1, prec1)), r):rst) 
        -- L,R,E but same precedence is issue with mixing 
        | assoc0 /= assoc1 && prec0 == prec1 = Left "Error mixing associativites"
        -- no fix with same precedence is bad
        | assoc0 == InfixE && prec0 == prec1 = Left "Error with no fix operator"

        -- If we should associate left. (either higher precedence, or equal precedence but left associative)
        | all id [assoc0 == InfixL, assoc1 == InfixL, prec0 == prec1] || prec0 > prec1 
            = return $  (l, (op1, r):rst)

        -- only case that remains is associating right
        | otherwise = do
            r' <-  go r
            (r'', rst') <- go' op1 r' rst
            let rexpr = Op (assoc1, prec1) l r''
            go' op0 rexpr rst'


-- runParser :: String -> Either P.ParseError Expr
-- runParser :: String -> Either String Expr
-- runParser :: String -> Either String String
runParser s = Bi.first show (P.parse (junk *> pExpr <* P.eof) "" s)  >>= fmap explicitBracket . resolveInfix
-- runParser s = Bi.first show (P.parse (junk *> pExpr <* P.eof) "" s)  >>= resolveInfix

junk :: Parser () 
junk = P.spaces

lexeme :: Parser a -> Parser a
lexeme pa = pa <* junk

pExpr :: Parser TokExprs
pExpr = (://) <$> pAtom <*> P.many go
  where
    go = do
        op <- lexeme $ P.between (P.char '`') (P.char '`') $ P.choice
                [ fmap (TokOp . (InfixL,)) (P.char 'l' *> pPrec)
                , fmap (TokOp . (InfixR,)) (P.char 'r' *> pPrec)
                , fmap (TokOp . (InfixE,)) (P.char 'e' *> pPrec)
                ]
        s <- pAtom
        return (op,s)

    pPrec = (read :: String -> Int) <$> P.many1 P.digit

pAtom :: Parser TokExprs
-- pAtom = fmap (flip (:// []) . (:// [])) pVar <|> P.between (lexeme (P.char '(')) (lexeme (P.char ')')) pExpr
-- pAtom = fmap TokPrim (lexeme (P.many1 P.letter))
pAtom = fmap TokPrim pVar
    <|> P.between (lexeme (P.char '(')) (lexeme (P.char ')')) pExpr

pVar :: Parser Expr
pVar = Var <$> lexeme (P.many1 P.letter)

