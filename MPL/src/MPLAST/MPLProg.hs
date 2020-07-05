{-# LANGUAGE TemplateHaskell #-}
module MPLAST.MPLProg where

import Optics.TH
import Optics.Prism
import Optics.Operators

newtype Prog stmt = Prog [stmt]

data Stmt defn = Stmt {
    _stmtDefn :: defn
    , _stmtDefns :: [defn]
    , _stmtWhereBindings :: [Stmt defn] 
}  

_MutuallyRecursiveStmts :: Prism' (Stmt defn) ((defn, defn), [defn])
_MutuallyRecursiveStmts = prism' embed match
  where
    embed ((a, b), rst) = Stmt a (b:rst) []
    match (Stmt a (b:bs) whs) = Just ((a,b), bs)
    match _ = Nothing

$(concat <$> traverse makeLenses 
    [ ''Stmt ]
 )
