{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module MPLPasses.AlphaRenameErrors where

import Optics


import MPLAST.MPLASTCore
import MPLAST.MPLProgI
import MPLAST.MPLProgII

import Control.Monad.Except

import Data.List.NonEmpty ( NonEmpty (..))
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
    ( MonadError e m
    , AsAlphaRenameErrors e ) =>
    [StmtI] -> m ()
overlappingStmtsCheck stmts = do
    undefined
    {-
    traverse overlappingHelper $ 
        concat 
            [ datacodata 
            , protocolcoprotocol
            , functions
            , processes
            ]
    return ()
  where
    overlappingHelper :: NonEmpty BnfcIdent -> m ()
    overlappingHelper (n :| ns) 
        | null ns = return ()
        | otherwise = throwError (review _OverlappingWhereDefs (n:ns))

    stmtdefnslist = stmts ^.. folded % stmtDefns % to NE.toList % to (map (view unDefnI))

    datacodata :: [NonEmpty BnfcIdent]
    datacodata = 
        NE.groupBy ((==) `on` (fst . stringPos))
        $ concat 
        $ mapMaybe (\a -> concat . map (\b -> b ^.. seqTypeClauseDecDefs  ) . NE.toList 
            <$> (preview _DataDefn a <> preview _CodataDefn a))
        $ concat 
        $ stmtdefnslist

    protocolcoprotocol :: [NonEmpty BnfcIdent]
    protocolcoprotocol = 
        NE.groupBy ((==) `on` (fst . stringPos))
        $ concat 
        $ mapMaybe (\a -> concat . map (\b -> b ^.. concTypeClauseDecDefs  ) . NE.toList 
            <$> (preview _ProtocolDefn a <> preview _CoprotocolDefn a))
        $ concat 
        $ stmtdefnslist

    functions = 
        NE.groupBy ((==) `on` (fst . stringPos))
        $ mapMaybe (\a -> view funName <$> preview _FunctionDecDefn a)
        $ concat
        $ stmtdefnslist

    processes = 
        NE.groupBy ((==) `on` (fst . stringPos))
        $ mapMaybe (\a -> view procName <$> preview _ProcessDecDefn a)
        $ concat
        $ stmtdefnslist
        -}
