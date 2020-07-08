{-# LANGUAGE TemplateHaskell #-}
module MPLAST.MPLProgTraversals where

import MPLAST.MPLProg
import Optics



{-
objectTypeDeclaredStateVars :: Traversal' (TypeClause phrase decdef a) a
objectTypeDeclaredStateVars = traversalVL trv
  where
    trv f n = n & traverseOf 
        typeClauseStateVar f
        -}

{- $>
<$ -}
    



{-
--defnDecDefTraversal :: Traversal' (Defn pattern letdef calldef decdef var concvar) decdef 
topLevelDefnDecDefTraversal :: Traversal
       (Defn pattern letdef calldef decdefn metavar var concvar)
       (Defn pattern letdef calldef decdefn' metavar var concvar)
       decdefn
       decdefn'
topLevelDefnDecDefTraversal = traversalVL trv
  where
    trv f (DataDefn lst) = DataDefn <$> traverse (traverseOf typeClauseDecDefTraversal f) lst
    trv f (CodataDefn lst) = CodataDefn <$> traverse (traverseOf typeClauseDecDefTraversal f) lst
    trv f (ProtocolDefn lst) = ProtocolDefn <$> traverse (traverseOf typeClauseDecDefTraversal f) lst
    trv f (CoprotocolDefn lst) = CoprotocolDefn <$> traverse (traverseOf typeClauseDecDefTraversal f) lst
    trv f (FunctionDecDefn n) = FunctionDecDefn <$> (n & traverseOf funName f)
    trv f (ProcessDecDefn n) = ProcessDecDefn <$> (n & traverseOf procName f)

{- $>
<$ -}

typeClauseDecDefTraversal ::
    Traversal 
        (TypeClausePhrase (phrase calldef var) decdef var) 
        (TypeClausePhrase (phrase calldef var) decdef' var)
        decdef decdef'
typeClauseDecDefTraversal = traversalVL trv
  where
    trv f (TypeClause a b c d) = 
        TypeClause <$> f a <*> pure b <*> pure c <*> traverse (traverseOf typePhraseDecDefTraversal f) d

typePhraseDecDefTraversal :: Traversal (TypePhrase phrase decdef var) (TypePhrase phrase decdef' var) decdef decdef'
typePhraseDecDefTraversal = traversalVL trv
  where
    trv f n = n & traverseOf typePhraseName f


{-=
-- seqTypeClauseDecDefs :: Traversal (SeqTypeClause def0 var) (SeqTypeClause def1 var) def0 def1
seqTypeClauseDecDefs  = traversalVL trv 
  where
    trv f (SeqTypeClause a b c d) = 
        SeqTypeClause <$> f a <*> pure b <*> pure c <*>
            traverse (traverseOf seqTypePhraseDecDefs f) d

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
        -}
        -}
