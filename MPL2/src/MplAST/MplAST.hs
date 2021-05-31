module MplAST.MplAST where

{-
newtype MplProg x = MplProg { _prog :: [MplStmt x] }
    deriving (Semigroup, Monoid )

data MplStmt x = MplStmt {
    _stmtDefns :: NonEmpty (MplDefn x)
    , _stmtWhereBindings :: [MplStmt x] 
}
-}
