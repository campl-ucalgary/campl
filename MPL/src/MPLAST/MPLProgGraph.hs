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

import Optics

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import GHC.Generics 

import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE

newtype ClausesKnot phrase ident = ClausesKnot {
        _clauseNeighbors :: NonEmpty (
                TypeClausePhrase 
                    (ClausesKnot phrase ident)
                    (ClausePhraseKnot (ClausesKnot phrase ident) phrase ident) 
                    phrase ident
            )
    }

newtype ClausePhraseKnot neighbors phrase ident = ClausePhraseKnot { 
        _phraseParent :: 
            TypeClausePhrase neighbors (ClausePhraseKnot neighbors phrase ident) phrase ident 
    } 

type TypePhraseKnot phrase ident = TypePhrase 
            (ClausePhraseKnot 
                (ClausesKnot phrase ident) 
                    phrase ident) 
            phrase ident

tieSpineGraph :: 
    TypeClausesPhrases () () phrase ident ->
    NonEmpty (TypeClause 
        (ClausesKnot phrase ident) 
        (TypePhrase (ClausePhraseKnot (ClausesKnot phrase ident) phrase ident) phrase ident)
        ident
    )
tieSpineGraph clauses = res
  where
    res = NE.map f clauses
    f clause = 
        let phrases' = (clause & typeClausePhrases % traversed % typePhraseContext .~ ClausePhraseKnot clause') ^. typeClausePhrases
            clause' = (clause & typeClauseNeighbors .~ ClausesKnot res) & typeClausePhrases .~ phrases' 
        in clause'

data SeqGraphPhrase ident =
    SeqDataGraphPhrase (TypePhraseKnot (DataPhrase 
        (SeqGraphPhrase ident) ident) ident)
    | SeqCodataGraphPhrase (TypePhraseKnot (CodataPhrase 
        (SeqGraphPhrase ident) ident) ident)

$(concat <$> traverse makeLenses 
    [ ''ClausesKnot
    , ''ClausePhraseKnot 
    ]
 )
$(concat <$> traverse makePrisms 
    [ ''SeqGraphPhrase
    ]
 )
