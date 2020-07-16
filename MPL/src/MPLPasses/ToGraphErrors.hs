{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -fno-warn-overlapping-patterns #-}
module MPLPasses.ToGraphErrors where

import MPLPasses.TypeClauseSanityErrors
import MPLPasses.TieTypeClause

import MPLPasses.UnificationErrors

import Optics
import MPLAST.MPLASTCore
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE 

import Data.Void

data TypeClauseError = 
    TypeClauseSanityCheck TypeClauseSanityCheckError
    | TieTypeClause TieTypeClauseError
  deriving Show

data FunctionError = 
    SeqPhraseNotInScope BnfcIdent
    -- pattern errors
    | ExpectedDataConstructor BnfcIdent
    | ExpectedCodataDestructor (NonEmpty BnfcIdent)
    | ExpectedDestructorsFromSameClause (NonEmpty BnfcIdent)
    | IllegalRecordPhrases (NonEmpty (BnfcIdent , ((), Pattern () () BnfcIdent)))
    -- pattern errors
    | ArityMismatch BnfcIdent Int Int
        -- expected n, but got m
        
    | ExpectedCaseDataConstructors (ExprI BnfcIdent)
    | ExpectedCaseSameConstructors (ExprI BnfcIdent)

  deriving Show

$(concat <$> traverse makeClassyPrisms 
    [ ''TypeClauseError
    , ''FunctionError ]
 )

instance AsTieTypeClauseError TypeClauseError where
    _TieTypeClauseError = _TieTypeClause

instance AsTypeClauseSanityCheckError TypeClauseError where
    _TypeClauseSanityCheckError = _TypeClauseSanityCheck

newtype ToGraphErrors = MkToGraphErrors (NonEmpty ToGraphError)
  deriving (Show, Semigroup)

data ToGraphError = 
    ToGraphTypeClauseError TypeClauseError
    | ToGraphFunctionError FunctionError
    | ToGraphUnificationError UnificationError
  deriving Show

$(concat <$> traverse makeClassyPrisms 
    [ ''ToGraphErrors
    , ''ToGraphError ]
 )

instance AsTypeClauseError ToGraphError where
    _TypeClauseError = _ToGraphTypeClauseError

instance AsTypeClauseSanityCheckError ToGraphError where
    _TypeClauseSanityCheckError = _TypeClauseError % _TypeClauseSanityCheck

instance AsTieTypeClauseError ToGraphError where
    _TieTypeClauseError = _TypeClauseError % _TieTypeClause

instance AsFunctionError ToGraphError where
    _FunctionError = _ToGraphFunctionError

instance AsUnificationError ToGraphError where
    _UnificationError = _ToGraphUnificationError
    

liftToGraphErrors err =
    _MkToGraphErrors # (err :| [])


{-
ToGraphError 
    (Defn 
        (NonEmpty TypeClauseError) 
         (NonEmpty TypeClauseError) 
        (NonEmpty TypeClauseError) 
        (NonEmpty TypeClauseError) 
        (NonEmpty FunctionError) 
        Void)
        -}

