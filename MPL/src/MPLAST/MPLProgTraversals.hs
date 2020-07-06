{-# LANGUAGE TemplateHaskell #-}
module MPLAST.MPLProgTraversals where

import MPLAST.MPLProg
import Optics

-- seqTypeClauseDecDefs :: Traversal (SeqTypeClause def0 var) (SeqTypeClause def1 var) def0 def1
seqTypeClauseDecDefs  = traversalVL trv 
  where
    trv f (SeqTypeClause a b c d) = 
        SeqTypeClause <$> f a <*> pure b <*> pure c <*>
            traverse (traverseOf seqTypePhraseDecDefs f) d
{- $>
:t seqTypePhraseDecDefs
<$ -}

-- seqTypePhraseDecDefs :: Traversal (SeqTypePhrase def0 var) (SeqTypePhrase def1 var) def0 def1
seqTypePhraseDecDefs  = traversalVL trv
  where
    trv f (SeqTypePhrase a b c) =
        SeqTypePhrase <$> f a <*> pure b <*> pure c

-- concTypeClauseDecDefs :: Traversal' (ConcTypeClause def var) def
concTypeClauseDecDefs = traversalVL trv
  where
    trv f (ConcTypeClause a b c d) = 
        ConcTypeClause <$> f a <*> pure b <*> pure c <*> 
            traverse (traverseOf concTypePhraseDecDefs f) d

-- concTypePhraseDecDefs :: Traversal' (ConcTypePhrase def var) def
concTypePhraseDecDefs = traversalVL trv
  where
    trv f (ConcTypePhrase a b c) = 
        ConcTypePhrase <$> f a <*> pure b <*> pure c
