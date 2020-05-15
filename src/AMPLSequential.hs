module AMPLSequential where

-- projet dependencies...
import AMPLTypes 
import AMPLEnv

-- base...
import Data.List
import Data.Array
import Control.Arrow

-- external dependencies...
import Control.Monad.Reader


isSequentialFinished :: ([Instr], [Val], [Val]) -> Bool
isSequentialFinished ([], [], [v]) = True
isSequentialFinished _ = False

stepSequential :: 
    ( MonadReader r m
    , HasSuperCombinators r ) => 
    SequentialInstr -> 
    ([Instr], [Val], [Val]) -> 
    m ([Instr], [Val], [Val])

stepSequential IStore (c, e, v:s) = return (c, v:e, s)

stepSequential (IAccess n) (c, e, s) = return (c, e, genericIndex e n : s)

stepSequential (ICall funid numargs) (c', e, s) 
    = do 
        env <- ask 
        return (superCombInstrLookup env funid, args, VClos (c', e) : s')
    where
        (args, s') = genericSplitAt numargs s

stepSequential IRet (c, e, v:VClos(c', e'):s) 
    = return (c', e', v : s)

stepSequential (ICons i n) (c, e, s) 
    = return (c, e, VCons (i, vs) : s')
    where
        (vs, s') = genericSplitAt n s

stepSequential (ICase arr) (c, e, VCons (i, vs) : s) 
    = return (arr ! i, vs ++ e, VClos (c,e) : s)

stepSequential (IRec arr) (c, e, s) 
    = return (c, e, VRec (arr, e) : s)

stepSequential (IDest i n) (c, e, VRec (cs, e') : s)
    = return (cs ! i, vs ++ e', VClos (c, e) : s')
    where
        -- in the CMachine, it reverses the vs, but in Prashant's code, he does
        -- not. When testing this, not reversing it is the correct thing to do
        (vs, s') = genericSplitAt n s

-- Constant instructions...
stepSequential (IConstInt k) (c, e, s) = return (c, e, VInt k : s)
stepSequential IAddInt (c, e, VInt n : VInt m : s) = return (c, e, VInt (n + m) : s)
stepSequential IMulInt (c, e, VInt n : VInt m : s) = return (c, e, VInt (n * m) : s)
stepSequential ILeqInt (c, e, VInt n : VInt m : s) = return (c, e, VBool (n <= m) : s)

stepSequential n mach = error ("Illegal sequential step with instruction:" ++ show n ++ "\nAnd machine: " ++ show mach)


-- Runs a sequential machine 
runSequential :: 
    ( MonadReader r m
    , HasSuperCombinators r ) =>
    [Instr] -> 
    m ( ([Instr], [Val], [Val]), [([Instr],[Val], [Val])])
runSequential cde
    = let m = (cde, [], []) in (m,) <$> fix f (return m)
    where
        f :: ( MonadReader r m , HasSuperCombinators r ) =>
                (m ([Instr], [Val], [Val]) -> m [([Instr], [Val], [Val])]) -> 
                m ([Instr], [Val], [Val]) -> m [([Instr], [Val], [Val])]
        f rec m 
            = do 
                m' <- m
                if isSequentialFinished m'
                    then return [m']
                    else case m' of
                            (SequentialInstr c : cs, e, v)
                                -> let m'' = stepSequential c (cs, e, v)
                                    in (:) <$> m'' <*> rec m''
                            _ -> error ("bad SequentialInstr" ++ show m')
