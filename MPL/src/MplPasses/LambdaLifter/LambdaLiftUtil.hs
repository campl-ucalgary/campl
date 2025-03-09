{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module MplPasses.LambdaLifter.LambdaLiftUtil where

import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import MplAST.MplCore
import MplAST.MplLambdaLifted
import MplAST.MplParsed
import MplAST.MplPatternCompiled
import MplAST.MplRenamed
import MplAST.MplTypeChecked
import Optics

type FunName = IdP MplLambdaLifted

type BoundArgs = Set (IdP MplLambdaLifted)

type FreeArgs = Set (IdP MplLambdaLifted)

-- | The data is as follows:
--    - Function name maps to:
--        - its function args,
--        - free variables used
--        - functions that are called within its body (not including the let declarations)
type CallGraph = Map FunName (BoundArgs, FreeArgs, Set FunName)

data LambdaLiftEnv = LambdaLiftEnv
  { -- | maps var idents to types
    _lambdaLiftTpMap :: Map (IdP MplLambdaLifted) (XMplType MplLambdaLifted),
    -- | the callgraph
    _lambdaLiftCallGraph :: CallGraph
  }

$(makeClassy ''LambdaLiftEnv)

type LambdaLift a b =
  forall m0 m r.
  ( MonadWriter [MplDefn MplLambdaLifted] m,
    Magnify m0 m (Map IdentT (MplAST.MplCore.MplType MplTypeChecked)) r,
    HasLambdaLiftEnv r
  ) =>
  a ->
  m b

getPattVarIdent ::
  MplPattern MplPatternCompiled ->
  IdP MplLambdaLifted
getPattVarIdent (PVar _ v) = v
