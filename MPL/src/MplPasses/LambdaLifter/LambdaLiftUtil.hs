{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module MplPasses.LambdaLifter.LambdaLiftUtil where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import MplAST.MplPatternCompiled
import MplAST.MplLambdaLifted

import Control.Monad.Writer

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map


type FunName = IdP MplLambdaLifted
type BoundArgs = Set (IdP MplLambdaLifted)
type FreeArgs = Set (IdP MplLambdaLifted)

{- | The data is as follows:
    - Function name maps to:
        - its function args, 
        - free variables used
        - functions that are called within its body (not including the let declarations)
-}
type CallGraph = Map FunName (BoundArgs, FreeArgs, Set FunName)

data LambdaLiftEnv = LambdaLiftEnv 
    { _lambdaLiftTpMap :: Map (IdP MplLambdaLifted) (XMplType MplLambdaLifted)
        -- ^ maps var idents to types
    , _lambdaLiftCallGraph :: CallGraph
        -- ^ the callgraph
    }

$(makeClassy ''LambdaLiftEnv)

type LambdaLift a b =
    forall m0 m r.
    ( MonadWriter [MplDefn MplLambdaLifted] m
    , Magnify m0 m (Map IdentT (MplAST.MplCore.MplType MplTypeChecked)) r
    , HasLambdaLiftEnv r ) =>
    a -> m b

getPattVarIdent :: 
    MplPattern MplPatternCompiled ->
    IdP MplLambdaLifted
getPattVarIdent (PVar _ v) =  v

