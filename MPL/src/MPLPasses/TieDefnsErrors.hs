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
  deriving Show

data FunctionError = 
    ExpectedDataConstructor BnfcIdent

    | ExpectedCodataDestructor BnfcIdent
    -- | Type clause name, type phrase..
    | ExpectedDestructorsFromSameClause (NonEmpty (BnfcIdent, BnfcIdent))
    -- | Record pattern matches require an exhaustive match of all
    -- the codata phrases. [BnfcIdent] is a list of missing phrases
    | NonExhaustiveRecordPhrases [BnfcIdent]
    -- not needed for type checking...
    -- | ExpectedPatternVar (Pattern () () BnfcIdent)

    -- | expected n, but got m
    | ArityMismatch BnfcIdent Int Int
        
    | ExpectedCaseDataConstructors (ExprI BnfcIdent)
    | ExpectedCaseSameConstructors (ExprI BnfcIdent)

    -- | TODO this needs to be a better error message i.e.
    -- we have [[BnfcIdent]] the list of phrases for which are from different
    -- graphs, but which graphs do they come from?
    | FoldUnfoldPhraseFromDifferentGraphs [[BnfcIdent]]
    | NonExhaustiveUnfoldClauses [BnfcIdent]
    | NonExhaustiveFold [BnfcIdent]

    -- | unfold errors
  deriving Show

$(concat <$> traverse makeClassyPrisms 
    [ ''TypeClauseError
    , ''FunctionError ]
 )


data TieDefnsError = 
    TieDefnTypeClauseError TypeClauseError
    | TieDefnFunctionError FunctionError
    | TieDefnUnificationError UnificationError

    | NotInScope BnfcIdent
    | DuplicatedDeclarations [BnfcIdent]
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
    

