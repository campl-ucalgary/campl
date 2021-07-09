module MplCliBenchRunner where

import Criterion

-- from the front end
import MplCliRunner.Runner
import MplCliRunner.Stack
import MplCliRunner.Flags

-- from benchmark 
import MplCliGen 

import Control.DeepSeq

import System.IO
import System.IO.Silently
import Control.Exception
import Data.List
import Control.Monad.IO.Class
import Control.Monad
import Control.Arrow
import Data.Coerce

import qualified Data.Map as Map


metaProgSubsAndProgToBenchmarks :: 
    [MetaValSubs] -> 
    MetaProg ->
    [Benchmark]
metaProgSubsAndProgToBenchmarks subs mprog = 
    map go subsandmprog
  where
    subsandmprog :: [(MetaValSubs, MetaProg)]
    subsandmprog = zip subs $ cycle [mprog]

    go (sub, mprog) = fprog 
        `deepseq` 
        ( bench 
            -- unforuntatley, just using show results in this
            -- generating invald json for the html output.. 
            -- pretty sure this is a bug and should probably
            -- go mention something to the authrtos...
            -- When I use the original show, it puts quotes in the 
            -- resulting string, which I'm pretty sure that's what
            -- the bug is.. strange that this bug is in the library however
            (intercalate ";" 
            $ map ( uncurry (++) <<< coerce *** ('=':) . id ) 
            $ Map.toList sub
            )
        $ nfIO 
        -- silence the output 
        -- (we don't really care about what it says.. yes, this techincally does effect the benchmark a bit)
        $ silence 
        $ execMplCli (cliRunPipelineInputProg fprog) jUSTRUNMPLCLIENV)
      where
        fprog = fillMetaProg sub mprog

            -- okay this is pretty bad...
            -- TODO: add an 'interact' sorta thing to the front
            -- end cli, so you can just pipe a file to it and
            -- it'll compile it..
            -- This is bad because we are also benchmarkign time
            -- it takes to make the file hahah. At the time of
            -- writing this, the size of the test cases should
            -- dominate the IO time however.. 
            --
            -- N.B. Compelted that badness..
            -- Ideally, we should honestly just compile the stuff, 
            -- then just run the machine ... I think that would be best....
            -- This woudl require shifting around the code in the runner
            -- to be a bit more modular instead of what it is now. 
            

jUSTRUNMPLCLIENV :: MplCliEnv 
jUSTRUNMPLCLIENV = MplCliEnv  
    { _mplCliFlags = [RunMplMach]
    , _mplCliInpFile = mempty
    }
    
 
-- cliRunner :: [String] -> IO ()
-- cliRunner args = getOpts args >>= execMplCli cliRunPipeline . review _MplCliEnv

