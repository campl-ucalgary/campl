{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MplParse.Stack where

import MplParse.Util

import Optics
import Optics.TH
import Optics.State.Operators

import qualified Text.Parsec as P
import Text.Parsec ((<|>))

import Data.Functor.Identity
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))

import Control.Monad.State

import Control.Exception
import Data.Maybe

import Debug.Trace

type IndentCol = Int
data IndentState = IndentState { 
    _indentErrorHint :: Maybe (Spanned String)
    , _absMode :: !Bool
    , _lowerBound :: !IndentCol
    , _upperBound :: !IndentCol
}

{-| showIndentCol. Small pretty printer for showing the indentation column -}
showIndentCol :: (IndentCol, IndentCol) -> String
showIndentCol (l, u)
    | u == maxUpperBoundIndentation = concat ["[", show l, ", ", "inf)"]
    | otherwise = concat ["[", show l, ", ", show u, "]"]

anyIndentState :: IndentState
anyIndentState = IndentState Nothing False minLowerBoundIndentation maxUpperBoundIndentation

minLowerBoundIndentation :: IndentCol
minLowerBoundIndentation = 1

maxUpperBoundIndentation :: IndentCol
maxUpperBoundIndentation = maxBound

$(makeLenses ''IndentState)

{-| The type of the MplParser. We use the IndentState to keep track of the required
state for indentation sensitive parsing (note this has to be part of Parsec's user state
since that will backtrack for us automatically)
-}
type MplParser a = P.Parsec String IndentState a

{-| This is an instance for MonadState in the user state of 'Parsec'
NOTE: We do this so we can use lenses in the user state -- honestly, this is more of a hack 
    and in the future, it's probably best to just give up on the lovely syntax of lenses
    and simply use Parsec's get and set state in the user state.
-}
instance {-# OVERLAPPING #-} Monad m => MonadState u (P.ParsecT s u m) where
    get = P.getState
    put = P.putState

-- * For running the parser...
testRunParser :: MplParser a -> String -> Either P.ParseError a
testRunParser pa inp = runParser pa "<test src file>" inp

runParser :: MplParser a -> P.SourceName -> String -> Either P.ParseError a
-- runParser pa src inp =  flip evalState anyIndentState $ P.runParserT pa () src inp
runParser pa src inp =  P.runParser pa anyIndentState src inp


{-| getIndent. This will get the current indentation
 -}
getIndent :: MplParser IndentCol
getIndent = fmap P.sourceColumn P.getPosition

{-| indentGt. Modifies a parser so that it must parse this token at strictly greater than indentation than its parent.
-}
indentGt :: MplParser a -> MplParser a
indentGt pa = guse absMode >>= \absmode -> if absmode then pa else do
    -- The initial indentation set is I
    -- sets indentation set to: { j \in \mathbb{N} : \exists i \in I s.t. j > i }
    oldlowerbound <- lowerBound <<%= succ
    upperBound .= maxUpperBoundIndentation
    indent <- getIndent 
    a <- pa
    -- The indentation set is J' now
    -- sets indentation set to: {i \in I : \exists j \in J' s.t. j > i }
    upperBound .= pred indent
    lowerBound .= oldlowerbound
    return a
    
{-| indentAny. Modifies a parser so that it must parse this token can have arbitrary indentation 
-}
indentAny :: MplParser a -> MplParser a
indentAny pa = guse absMode >>= \absmode -> if absmode then pa else do
    oldlowerbound <- lowerBound <<.= minLowerBoundIndentation
    oldupperbound <- upperBound <<.= maxUpperBoundIndentation
    a <- pa
    lowerBound .= oldlowerbound 
    upperBound .= oldupperbound 
    return a

-- * TODO: we can add the other relations for indentation sensitivity, but it doesn't really matter since we don't use them..


{-| indentTerminal. Modifies a parser so that it handles the indentation of a terminal in the grammar.
-}
indentTerminal :: MplParser a -> MplParser a
{- old version that more clearly gives the idea -- assuming that @guardIndent@ is essentially @guard@ (bad error messages).
indentTerminal pa = do 
    indent <- getIndent 
    guardIndent 
    lowerBound .= indent 
    upperBound .= indent 
    absMode .= False 
    pa
-}
indentTerminal pa = do 
    indent <- getIndent 
    ist <- guse equality 
    lowerBound .= indent 
    upperBound .= indent 
    absMode .= False 
    pa <* guardIndent indent ist

{-| guardIndent. This will check if the current indentation is valid, and fail 
otherwise.
NOTE: This should normally be run AFTER the parser is run to give better error messages.
Hence, we should feed it the @ident@ of the position, and the @ist@ prior of consuming the token.
NOTE: This is /should/ only be used in 'indentTerminal'
-}
guardIndent :: 
    IndentCol -> 
    IndentState -> 
    MplParser ()
guardIndent ident IndentState{_lowerBound = l, _upperBound = u} = do
    -- ident <- getIndent 
    -- l <- guse lowerBound
    -- u <- guse upperBound
    if l <= ident && ident <= u
        then return ()
        else do
            hint <- guse indentErrorHint
            P.unexpected $ concat 
            -- fail $ concat 
                [ "indentation of "
                , show ident
                , " but expected indentation to in the interval "
                , showIndentCol (l,u)
                , case hint of 
                    Just hint' -> concat
                        [ " from @" 
                        , hint' ^. spanned 
                        , "@ at "
                        , hint' ^. spannedSpan % lowerSpan % to show
                        , ""
                        ]
                    Nothing -> ""
                ]
{-
indentTerminalAnnotated :: MplParser (Spanned String) -> MplParser (Spanned String)
indentTerminal pa = do 
    indent <- getIndent 
    guardIndent 
    lowerBound .= indent 
    upperBound .= indent 
    absMode .= False 
    pa
    -}

{-| indentAbsAlign. Sets the absolute indentation flag to true. -}
indentAbsAlign :: MplParser a -> MplParser a
indentAbsAlign pa = absMode .= True >> pa

infix 0 <?^>
{-| (<?^>). This labels a parser with an indentation hint. This is similar to '(Text.Parsec.<?>)' but 
differs since:
    - '(Text.Parsec.<?>)' will only add the error if the parser does not consume any input;
    - (<?^>) will always add the hint, and remove the hint after the parser is finished running; and
    - indentation hints internally uses the "unexpected" part of Text.Parsec to report the error (see 'guardIndent')
-}
(<?^>) :: MplParser a -> Spanned String -> MplParser a
(<?^>) pa sstr = do
    indentErrorHint .= Just sstr
    a <- pa
    indentErrorHint .= Nothing
    return a


{-| indentBlock. This gives us an indentation block similar to Haskell.  -}
indentBlock :: 
    forall a open close sep.
    -- | Parser for the layout keyword (e.g. @do@ in Haskell)
    MplParser (Spanned String) -> 
    -- | Parser for the optional open bracket of an indented block (e.g. @{@ in Haskell) 
    -- (be sure to make this a terminal expression as this changes the indentation set)
    MplParser open -> 
    -- | Parser for the optional close bracket of an indented block (e.g. @}@ in Haskell) 
    -- (be sure to make this a terminal expression as this changes the indentation set)
    MplParser close -> 
    -- | Parser for one line of the indented block
    MplParser a -> 
    -- | Parser for the optional separators of the indented block (e.g. @;@ in Haskell) 
    -- (be sure to make this a terminal expression as this changes the indentation set)
    MplParser sep -> 
    -- | Returns: (Parsed layout keyword, list of parses in the indented block)
    MplParser (Spanned String, [a])
indentBlock playoutword popenbrace pclosebrace pindentline psep = do
    layoutword <- playoutword
    identlines <- explicitLayout <|> implicitLayout <?^> layoutword
    return (layoutword, identlines)
  where
    explicitLayout = indentGt popenbrace *> indentAny pindentlines <* indentAny pclosebrace
    implicitLayout = indentGt (fmap concat $ P.many $ indentAbsAlign pindentlines)

    pindentlines :: MplParser [a]
    pindentlines = do
        indentline <- fmap maybeToList $ pindentlines' <|> fmap Just pindentline 
        indentlines <- fmap catMaybes $ P.many pindentlines'
        return $ indentline ++ indentlines
    -- pindentlines' = indentMany1 (indentGt psep) >> pindentline 
    {- TODO: need to fix the following parse issues:
        @
        do
            a;
            b
        @
        this is fixed

        @
        do
            a;
            b;
        @
        will also error -- this needs some more investigation. I think
        to fix this, we need to handle some extra edges cases when we reach the 
        end of the input.

    -}
    pindentlines' :: MplParser (Maybe a)
    pindentlines' = indentGt psep >> (fmap Just pindentline <|> return Nothing)
        

traceBounds :: MplParser ()
traceBounds = do
    l <- guse lowerBound
    u <- guse upperBound
    absmode <- guse absMode
    traceM $ showIndentCol (l, u)
    traceM $ "AbsMode: " ++ show absmode
