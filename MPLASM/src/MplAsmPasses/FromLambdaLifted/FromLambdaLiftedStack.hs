{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MplAsmPasses.FromLambdaLifted.FromLambdaLiftedStack where

import Optics

-- Front end
import qualified MplPasses.Passes as Passes
import qualified MplPasses.Parser.BnfcParse as B
import qualified MplPasses.PassesErrors as PassesErrors
import qualified MplPasses.PassesErrorsPprint as PassesErrors
import MplAST.MplCore 
import MplAST.MplTypeChecked 
import MplUtil.UniqueSupply

-- Usual monad
import Control.Monad.State
import Control.Monad.Writer

-- containers
import Data.Map (Map)
import qualified Data.Map as Map

{- TODO: there are some instances where memoizing some calls would be helpful,
 - but never really got around to implementing this... Actually this would
 - really be an optimization I guess. never really did this though.
 -}

newtype FromLambdaLiftedStack e a = FromLambdaLiftedStack (WriterT [e] (State FromLambdaLiftedState) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadWriter [e] 
    , MonadState FromLambdaLiftedState
    )

data FromLambdaLiftedState = FromLambdaLiftedState 
    { _lambdaLiftedUniqSupply :: UniqueSupply
    , _lambdaLiftedCallMemoizer :: Map (IdP MplLambdaLifted, [IdP MplLambdaLifted]) Int
    }
