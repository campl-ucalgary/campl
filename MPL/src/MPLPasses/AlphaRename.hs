{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module MPLPasses.AlphaRename where

import Optics
import Optics.Lens
import Optics.State.Operators
import Optics.View
import Data.Tuple.Optics
import Data.Functor.Foldable

import MPLAST.MPLASTCore
import MPLAST.MPLProgI
import MPLAST.MPLProgII

import MPLPasses.AlphaRenameErrors

import MPLUtil.Data.Tuple.Optics
import MPLUtil.Data.Either.AccumEither

import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.List.NonEmpty ( NonEmpty (..)) 
import qualified Data.List.NonEmpty as NE

import Data.Either
import Control.Arrow

import Control.Monad.State
import Control.Monad.Except

import MPLCompile

data AlphaRenameState = AlphaRenameState  {
    _varIdentTag :: UniqueTag
    , _defIdentTag :: UniqueTag
    , _scopeStack :: [[(String, UniqueTag)]]
    , _concScope :: [(String, UniqueTag)]
}
$(makeClassy ''AlphaRenameState)

freshIdentTag :: 
    ( MonadState c m
    , HasAlphaRenameState c ) => 
    Optics.Lens.Lens' c UniqueTag ->
    m UniqueTag
freshIdentTag l = 
    l <<%= succ

withScope ::
    ( MonadState c m
    , HasAlphaRenameState c ) => 
    [(String, UniqueTag)] ->
    m a -> m ()
withScope scope action = do
    scopeStack %= (scope:)
    action
    scopeStack %= tail

scopeLookup ::
    ( MonadState c m
    , HasAlphaRenameState c ) => 
    String ->
    m (Maybe UniqueTag)
scopeLookup val = 
    guses scopeStack (lookup val . concat) 

{-
alphaRenameStep ::
    ( MonadError e m
    , MonadState s m
    , AsAlphaRenameErrors e ) =>
    StmtI -> m StmtII
alphaRenameStep stmt = do
    undefined

alphaRenameStmt ::
    ( MonadError e m
    , MonadState s m
    , HasAlphaRenameState s
    , AsAlphaRenameErrors e ) =>
    StmtI -> m ([(String, UniqueTag)], StmtII)
alphaRenameStmt stmt = do
    -- check for conflicting definitions
    overlappingStmtsCheck (stmt ^. stmtWhereBindings)

    (wscopes, wstmtiis) <- foldMap (second (:[])) 
        <$> traverse alphaRenameStmt (stmt ^. stmtWhereBindings)

    withScope wscopes $ do
        undefined

    return undefined

alphaRenameDefn ::
    ( MonadError e m
    , MonadState s m
    , AsAlphaRenameErrors e ) =>
    DefnI ->
    m DefnII
alphaRenameDefn = undefined
-}
