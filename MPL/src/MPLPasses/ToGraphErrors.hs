{-# LANGUAGE TemplateHaskell #-}
module MPLPasses.ToGraphErrors where

import MPLPasses.TypeClauseSanityErrors
import MPLPasses.TieTypeClause

import Optics
import MPLAST.MPLASTCore
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE 

import Data.Void

data TypeClauseError = 
    TypeClauseSanityCheck TypeClauseSanityCheckError
    | TieTypeClause TieTypeClauseError

data FunctionError = 
    SeqPhraseNotInScope BnfcIdent
    | ExpectedDataConstructor BnfcIdent
    | ArityMismatch BnfcIdent Int Int
        -- expected n, but got m

$(concat <$> traverse makeClassyPrisms 
    [ ''TypeClauseError
    , ''FunctionError ]
 )

instance AsTieTypeClauseError TypeClauseError where
    _TieTypeClauseError = _TieTypeClause

instance AsTypeClauseSanityCheckError TypeClauseError where
    _TypeClauseSanityCheckError = _TypeClauseSanityCheck



newtype ToGraphErrors = ToGraphError 
    (Defn 
        (NonEmpty TypeClauseError) 
         (NonEmpty TypeClauseError) 
        (NonEmpty TypeClauseError) 
        (NonEmpty TypeClauseError) 
        (NonEmpty FunctionError) 
        Void)

$(concat <$> traverse makeClassyPrisms 
    [ ''ToGraphErrors ]
 )

liftFunctionError err = _ToGraphError # _FunctionDecDefn # (err :| [])
