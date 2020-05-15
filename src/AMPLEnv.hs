module AMPLEnv where

import Data.Array
import Data.List

import AMPLTypes

class HasSuperCombinators a where
    superCombInstrLookup :: a -> FunID -> [Instr]
    superCombNameLookup :: a -> FunID -> String

class HasLog a where
    getLog :: a -> (String -> IO ())


-- Environment that the machine runs in the entire time
-- includes: supercombinator defintions, data for locks and queues
data AmplEnv = AmplEnv
    {
        supercombinators :: Array FunID (String, [Instr])
            -- function definitions (supercombinators is the terminolgy Simon Peyton Jones uses)
        , amplLog :: String -> IO ()
    }

-- smart constructor for the environment
amplEnv :: [(String, [Instr])] -> 
    (String -> IO ()) ->
    AmplEnv
amplEnv [] g = 
    AmplEnv
        (array (FunID 1, FunID 0) [])
        g
amplEnv defs g = 
    AmplEnv
        (array (FunID 0, FunID (genericLength defs - 1)) (zip [FunID 0 .. ] defs))
        g

instance HasSuperCombinators AmplEnv where
    {-# INLINE superCombInstrLookup #-}
    superCombInstrLookup env ix = snd (supercombinators env ! ix)
    superCombNameLookup env ix = fst (supercombinators env ! ix)

instance HasLog AmplEnv where
    getLog = amplLog
