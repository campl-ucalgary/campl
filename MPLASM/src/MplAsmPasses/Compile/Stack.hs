{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module MplAsmPasses.Compile.Stack where

import Optics
import Optics.State.Operators

import Data.Coerce

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

import MplAsmAST.MplAsmCore
import MplMach.MplMachTypes

data SymEntry x 
    = SymProc CallIx
    | SymConcObj (Map (IdP x) HCaseIx)

data MplAsmCompileSt x = MplAsmCompileSt 
    { _varStack :: [IdP x]
    , _channelTranslations :: Map (IdP x) (Polarity, LocalChan)

    , _uniqCounters :: MplAsmCompileStUniqs

    , _symTab :: MplAsmCompileStSymTabs x
    }

data MplAsmCompileStUniqs = MplAsmCompileStUniqs 
    { _uniqLocalChan :: LocalChan
    -- | recall that service channels are negative.
    , _uniqServiceChan :: LocalChan
    , _uniqCallIx :: CallIx
    }

data MplAsmCompileStSymTabs x = MplAsmCompileStSymTabs 
    { _symTabFuns :: Map (IdP x) (CallIx, Int)
        -- ^ function name --> (function id,number of args)
    , _symTabProcs :: Map (IdP x) (CallIx, (Int, [LocalChan], [LocalChan]))
        -- ^ function name --> (function id,(number of seq args, inchs, outchs))

    , _symTabData :: Map (IdP x)  (Map (IdP x) (CaseIx, Int))
        -- ^ data name --> (caseix ,number of args)
    , _symTabCodata :: Map (IdP x)  (Map (IdP x) (CaseIx, Int))
        -- ^ codata name --> (caseix ,number of args)
    , _symTabProtocol :: Map (IdP x) (Map (IdP x) HCaseIx)
        -- ^ protocol name --> hcaseix
    , _symTabCoprotocol :: Map (IdP x) (Map (IdP x) HCaseIx)
        -- ^ coprotocol name --> hcaseix
    }


$(makeLenses ''MplAsmCompileSt )
$(makeLenses ''MplAsmCompileStUniqs)
$(makeLenses ''MplAsmCompileStSymTabs)

localMplAsmCompileSt ::
    ( MonadState (MplAsmCompileSt x) m ) =>
    m a ->
    m a 
localMplAsmCompileSt ma = do
    oldst <- guse equality
    a <- ma
    nst <- guse equality
    equality .= set uniqCounters (nst ^. uniqCounters) oldst
    return a

initMplAsmCompileSt :: MplAsmCompileSt x
initMplAsmCompileSt = MplAsmCompileSt 
    { _varStack = []
    , _channelTranslations = Map.empty

    , _uniqCounters = MplAsmCompileStUniqs 
        { _uniqLocalChan = coerce (0 :: Int)
        , _uniqServiceChan = coerce (-10 :: Int)
        , _uniqCallIx = coerce (0 :: Int)
        }
    , _symTab = initMplAsmCompileStSymTabs 
    }

initMplAsmCompileStSymTabs :: MplAsmCompileStSymTabs x
initMplAsmCompileStSymTabs = MplAsmCompileStSymTabs 
    { _symTabFuns = Map.empty
    , _symTabProcs = Map.empty

    , _symTabData = Map.empty
    , _symTabCodata = Map.empty
    , _symTabProtocol = Map.empty
    , _symTabCoprotocol = Map.empty
    }
