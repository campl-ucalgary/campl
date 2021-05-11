module MplPasses.PassesErrorsPprint 
    ( module Data.Text.Prettyprint.Doc
    , MplDoc
    , MplAnn (..)
    , pprintIdentPWithLoc
    , backticksinglequote
    , codeblock
    , pprintIdentPLoc 
    , pprintLoc 
    , pprintSpan 
    )
    where
{- | Rexports for pretty printing using 'Data.Text.Prettyprint.Doc'. 
Moreover, this includes some useful functions for converting parts of the 'MplAST' into the @MplDoc@ type.
-}

import MplAST.MplCore
import Data.Text.Prettyprint.Doc
import Optics


type MplDoc = Doc MplAnn

{- | MplAnn. Thisis the annotation for @MplDoc@. Currently, we do not use this.  -}
data MplAnn = MplAnn

{-| this will surround a @Doc ann@ with a backtick and a single quote (this is to copy BNFC's error messages) -}
backticksinglequote :: Doc ann -> Doc ann
backticksinglequote = enclose (pretty '`') (pretty '\'')

{-| this is to pretty print a code block -}
codeblock :: String -> Doc ann
codeblock blk = 
    line 
    -- <> pretty "``"
    <> line
    <> indent' (pretty blk)
    <> line
    -- <> pretty "''"
    <> line
  where
    indent' = indent 4
 
{- | pretty prints and 'IdentP' and writes its location information afterwards -}
pprintIdentPWithLoc :: IdentP -> MplDoc
pprintIdentPWithLoc idp = 
    (backticksinglequote $ pretty $ idpnameocc ^. nameOccName % (coerced :: Iso' Name String))
    <+> pretty "at" <+> pprintIdentPLoc idp
  where
    idpnameocc = idp ^. identPNameOcc


{- | prints essentially @at line __ and column __@ of an 'IdentP' -}
pprintIdentPLoc :: IdentP -> MplDoc
pprintIdentPLoc idp = hsep locationp
  where
    idpnameocc = idp ^. identPNameOcc

    locationp = case idpnameocc ^. nameOccLocation of
        Location (row,col) -> 
            [ pretty "line"
            , pretty row
            , pretty "and column"
            , pretty col
            ]

{- | prints essentially @line __ and column __@. TODO: duplicated code from above -}
pprintLoc :: Location -> MplDoc
pprintLoc loc = hsep locationp
  where
    locationp = case loc of
        Location (row,col) -> 
            [ pretty "line"
            , pretty row
            , pretty "and column"
            , pretty col
            ]

{- | prints essentially @row __ and column __ to row __ and column __@. TODO: duplicated code from above -}
pprintSpan :: Span -> MplDoc
pprintSpan span = spanp
  where
    spanp = case span of
        Span (lspan,rspan) -> pprintLoc lspan <+> pretty "to" <+> pprintLoc rspan




