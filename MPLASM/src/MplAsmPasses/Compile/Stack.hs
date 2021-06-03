module MplAsmPasses.Compile.Stack where

import Optics
import Optics.State.Operators
import AMPLTypes

import Data.Coerce

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

import MplAsmAST.MplAsmCore

data SymEntry x 
    = SymProc FunID
    | SymConcObj (Map (IdP x) HCaseIx)

data MplAsmCompileSt x = MplAsmCompileSt 
    { _varStack :: [IdP x]
    , _channelTranslations :: Map (IdP x) (Polarity, LocalChanID)

    , _uniqLocalChanId :: LocalChanID
    , _uniqFunId :: FunID

    , _symTabFuns :: Map (IdP x) (FunID, Word)
        -- ^ function name --> (function id,number of args)
    , _symTabProcs :: Map (IdP x) (FunID, (Word, [LocalChanID], [LocalChanID]))
        -- ^ function name --> (function id,(number of seq args, inchs, outchs))

    , _symTabData :: Map (IdP x)  (Map (IdP x) (CaseIx, Word))
        -- ^ data name --> (caseix ,number of args)
    , _symTabCodata :: Map (IdP x)  (Map (IdP x) (CaseIx, Word))
        -- ^ codata name --> (caseix ,number of args)
    , _symTabProtocol :: Map (IdP x) (Map (IdP x) HCaseIx)
        -- ^ protocol name --> hcaseix
    , _symTabCoprotocol :: Map (IdP x) (Map (IdP x) HCaseIx)
        -- ^ coprotocol name --> hcaseix
    }

$(makeLenses ''MplAsmCompileSt )

localMplAsmCompileSt ::
    ( MonadState (MplAsmCompileSt x) m ) =>
    m a ->
    m a 
localMplAsmCompileSt ma = do
    oldst <- guse equality
    a <- ma
    nst <- guse equality
    equality .= 
        ( set uniqLocalChanId (nst ^. uniqLocalChanId) 
        . set uniqFunId (nst ^. uniqFunId) 
        ) oldst
    return a

initMplAsmCompileSt :: MplAsmCompileSt x
initMplAsmCompileSt = MplAsmCompileSt 
    { _varStack = []
    , _channelTranslations = Map.empty

    , _uniqLocalChanId = coerce (0 :: ChannelIdRep)
    , _uniqFunId = coerce (0 :: Word)

    , _symTabFuns = Map.empty
    , _symTabProcs = Map.empty

    , _symTabData = Map.empty
    , _symTabCodata = Map.empty
    , _symTabProtocol = Map.empty
    , _symTabCoprotocol = Map.empty
    }
