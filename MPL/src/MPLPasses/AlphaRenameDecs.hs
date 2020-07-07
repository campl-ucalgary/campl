{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module MPLPasses.AlphaRenameDecs where

import Optics
import Optics.Lens
import Optics.State.Operators
import Optics.View
import Data.Tuple.Optics
import Data.Functor.Foldable

import MPLAST.MPLASTCore
import MPLAST.MPLProgI
import MPLAST.MPLProgII

import MPLPasses.AlphaRenameDecsErrors

import MPLUtil.Data.Tuple.Optics
import MPLUtil.Data.Either.AccumEither

import Data.Map (Map (..))
import qualified Data.Map as Map
import Data.List.NonEmpty ( NonEmpty (..)) 
import qualified Data.List.NonEmpty as NE

import Data.Either
import Data.Maybe
import Control.Arrow

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.RWS

import MPLCompile

data AlphaRenameState = AlphaRenameState  {
    _defIdentTag :: UniqueTag
    , _decsStack :: [[(String, (DefnII, UniqueTag))]]
    , _renamedSymbolTable :: Map UniqueTag DefnII
}

$(makeClassy ''AlphaRenameState)

freshIdentTag :: 
    ( MonadState c m
    , HasAlphaRenameState c ) => 
    m UniqueTag
freshIdentTag  = 
    defIdentTag <<%= succ

withScope ::
    ( MonadState c m
    , HasAlphaRenameState c ) => 
    [(String, (DefnII, UniqueTag))] ->
    m a -> m a
withScope scope action = do
    decsStack %= (scope:)
    a <- action
    decsStack %= tail
    return a

scopeLookup :: 
    ( MonadState s m
    , HasAlphaRenameState s
    , Is k A_Fold) =>
    String -> 
    Optic' k is DefnII a -> 
    m (Maybe (DefnII, UniqueTag))
scopeLookup val prism = do 
    symtable <- guse decsStack
    return $ snd <$> findOf (folded % folded ) 
                    (\(a, (b, c)) -> a == val && has prism b) symtable

-- remember to do a large overlappingStmtsCheck over the whole program..
alphaRenameStep ::
    ( MonadError (NonEmpty e) m
    , MonadState s m
    , AsAlphaRenameErrors e ) =>
    StmtI -> m StmtII
alphaRenameStep stmt = do
    undefined

alphaRenameStmt ::
    ( MonadError (NonEmpty e) m
    , MonadState s m
    , HasAlphaRenameState s
    , AsAlphaRenameErrors e ) =>
    StmtI -> m ([(String, (DefnII,UniqueTag))], StmtII)
alphaRenameStmt stmt = do
    -- check for conflicting definitions
    overlappingStmtsCheck (stmt ^. stmtWhereBindings)

    -- get the where binding defs
    (ndecmap, wstmtiis) <- foldMap (second (:[])) 
        <$> traverse alphaRenameStmt (stmt ^. stmtWhereBindings)

    undefined
    -- alpha rename definitions..
    {-
    ndefs <- withScope wscopes $ do
                defs <- mapM alphaRenameDefn (stmt ^. stmtDefns)
                return defs
    ndefs' <- liftAccumEither $ sequenceA ndefs
        -- there were possibly errors, throw them now

    -- return the new statement with the scope that extends
    -- toe the global context
    return $ (wscopes, Stmt ndefs' wstmtiis)
    -}

{-
alphaRenameDefn ::
    forall s m e.
    ( MonadState s m
    , HasAlphaRenameState s
    , AsAlphaRenameErrors e ) =>
    DefnI ->
    m (AccumEither (NonEmpty e) DefnII)
alphaRenameDefn n = case n ^. unDefnI of
    DataDefn n -> do 
        ndefs <- traverse f n 
        return $ (DefnII . DataDefn <$> sequenceA ndefs)
      where
        f :: TypeClausePhrase (DataPhrase BnfcIdent BnfcIdent) BnfcIdent BnfcIdent -> 
            m (AccumEither (NonEmpty e) 
                (TypeClausePhrase (DataPhrase DefIdent VarIdent) DefIdent VarIdent))
        f = undefined

    
-- alphaRenameDefn (DataDefn ) = undefined
-}

