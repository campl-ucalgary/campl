{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS -fno-warn-overlapping-patterns #-}
module MPLPasses.TieDefnsErrors where


import MPLPasses.UnificationErrors

import Optics
import MPLAST.MPLASTCore
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE 

import Data.Void

data TypeClauseError = 
    InvalidMutuallyRecursiveTypeArgDec (NonEmpty (TypeClause () () () BnfcIdent BnfcIdent))
    | OverlappingTypeVariables (NonEmpty (TypeClause () () () BnfcIdent BnfcIdent))
    | CodataInputArgStateVarOccurence (TypeClause () () () BnfcIdent BnfcIdent)
    | PhraseToMustBeStateVar (TypeClause () () () BnfcIdent BnfcIdent)
    | TypeNotInScope BnfcIdent
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


data TieDefnsError = 
    TieDefnTypeClauseError TypeClauseError
    | TieDefnFunctionError FunctionError
    | TieDefnUnificationError UnificationError

    -- TODO -- change the symbol table
    -- so it keeps the position infromation
    -- and make this take a list of [BnfcIdent]
    | AmbiguousLookup 
  deriving Show

$(concat <$> traverse makeClassyPrisms 
    [ ''TieDefnsError ]
 ) 

instance AsFunctionError TieDefnsError where
    _FunctionError = _TieDefnFunctionError

instance AsTypeClauseError TieDefnsError where
    _TypeClauseError = _TieDefnTypeClauseError

instance AsUnificationError TieDefnsError where
    _UnificationError = _TieDefnUnificationError
    
