{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module MPLAST.MPLProgGraph where

import MPLAST.MPLProg
import MPLAST.MPLTypeAST

import Optics

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import Data.Void

import GHC.Generics 

import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE

newtype ClausesKnot calldef ident = ClausesKnot {
    _clauseNeighbors :: [(TypeClause  
            (ClausesKnot calldef ident)
            (ClausePhraseKnot (ClausesKnot calldef ident) calldef ident)
            calldef ident
        )]
}  deriving (Show,Eq)

newtype ClausePhraseKnot neighbors calldef ident = ClausePhraseKnot { 
        _phraseParent :: TypeClause 
            neighbors 
            (ClausePhraseKnot neighbors calldef ident)
            calldef
            ident
    }   deriving (Show, Eq)

type TypeClauseKnot calldef ident =
    TypeClause 
        (ClausesKnot calldef ident) 
        (ClausePhraseKnot (ClausesKnot calldef ident) calldef ident)
        calldef 
        ident


testClause = TypeClause 
    'a' [] 'b' [TypePhrase () 'z' [] (TypeVar 'y')] ()
    



{-

-- type TypeClausePhraseKnot (TypeClause (ClausesKnot phrase ident) (TypePhrase (ClausePhraseKnot (ClausesKnot phrase ident) phrase ident) phrase ident) ident)

type TypePhraseKnot phrase ident = TypePhrase 
            (ClausePhraseKnot 
                (ClausesKnot phrase ident) 
                    phrase ident) 
            phrase ident

type TypeClauseKnot phrase ident = TypeClausePhrase 
    (ClausesKnot phrase ident) 
    (ClausePhraseKnot (ClausesKnot phrase ident) phrase ident) 
    phrase ident

type TypeClausePhraseKnots phrase ident = NonEmpty (TypeClauseKnot phrase ident)

tieSpineGraph :: 
    TypeClausesPhrases () () phrase ident ->
    TypeClausePhraseKnots phrase ident
tieSpineGraph clauses = res
  where
    res = NE.map f clauses
    f clause = 
        let phrases' = (clause & typeClausePhrases % traversed % typePhraseContext .~ ClausePhraseKnot clause') ^. typeClausePhrases
            clause' = (clause & typeClauseNeighbors .~ ClausesKnot res) & typeClausePhrases .~ phrases' 
        in clause'

{-
data SeqPhraseG ident =
    SeqPhraseGData (TypePhraseKnot (TypePhraseFromTo 
        (SeqPhraseG ident) ident) ident)
    | SeqPhraseGCodata (TypePhraseKnot (CodataPhrase 
        (SeqGraphPhrase ident) ident) ident)
        -}
        
data SeqClauseG ident =
    SeqDataClauseG (TypeClauseKnot (TypePhraseFromTo 
        (SeqClauseG ident) ident) ident)
    | SeqCodataClauseG (TypeClauseKnot (TypePhraseFromTo 
        (SeqClauseG ident) ident) ident)

type SeqObjectG ident = TypeClausePhraseKnots (TypePhraseFromTo (SeqClauseG ident) ident) ident

newtype DefnG ident = DefnG {
        unDefnG :: Defn (SeqObjectG ident) (SeqObjectG ident) Void Void Void Void
    }


$(concat <$> traverse makeLenses 
    [ ''ClausesKnot
    , ''ClausePhraseKnot 
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''SeqClauseG
    ]
 )
-}
