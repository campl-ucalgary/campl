{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module MPLPasses.AlphaRename.PrgObjectsErrors where

import Optics 

import MPLAST.MPLASTCore
import MPLAST.MPLProgI
import MPLAST.MPLProgII

import MPLUtil.Data.Either.AccumEither

import Control.Monad.Except

import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE
import Control.Arrow
import Data.Function
import Data.Maybe

data AlphaRenameErrors = 
    FreeVariable BnfcIdent
    | OverlappingVariables [BnfcIdent]
    | OverlappingWhereDefs [BnfcIdent]


$(makeClassyPrisms ''AlphaRenameErrors)

overlappingStmtsCheck ::
    forall e m.
    ( MonadError (NonEmpty e) m
    , AsAlphaRenameErrors e ) =>
    [StmtI] -> m ()
overlappingStmtsCheck stmts = do
    liftAccumEither $ traverse (liftAEither . overlappingHelper) $ concatMap 
        (NE.groupBy ((==) `on` fst . stringPos))
            [ datadecs ++ codatadecs
            , protocoldecs ++ coprotocoldecs
            , functionsdecs
            , protocoldecs
            ]
    return ()
  where
    overlappingHelper :: NonEmpty BnfcIdent -> Either (NonEmpty e) ()
    overlappingHelper (n :| ns) 
        | null ns = Right ()
        | otherwise = Left (review _OverlappingWhereDefs (n:ns) :| [])

    defnFocus = folded % stmtDefns % folded % unDefnI
    typeClauseDecFocus = folded % typeClauseDecDefTraversal

    codatadecs :: [BnfcIdent]
    codatadecs = stmts & foldOf ( 
            defnFocus % _CodataDefn % typeClauseDecFocus % to pure
        )

    datadecs :: [BnfcIdent]
    datadecs = stmts & foldOf ( 
            defnFocus % _DataDefn % typeClauseDecFocus % to pure
        )

    protocoldecs :: [BnfcIdent]
    protocoldecs = stmts & foldOf (
            defnFocus % _ProtocolDefn % typeClauseDecFocus % to pure
        )

    coprotocoldecs :: [BnfcIdent]
    coprotocoldecs = stmts & foldOf (
            defnFocus % _CoprotocolDefn % typeClauseDecFocus % to pure
        )

    functionsdecs :: [BnfcIdent]
    functionsdecs = 
        stmts & foldOf ( defnFocus % _FunctionDecDefn % funName  % to pure)
 

