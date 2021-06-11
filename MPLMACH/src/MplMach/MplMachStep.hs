{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}
module MplMach.MplMachStep where

import Optics
import Data.Array
import MplMach.MplMachTypes
import MplMach.MplMachStack

seqStep :: 
    ( Applicative m ) =>
    -- | continuation with what to do next if all sequential pattern
    -- matches fails
    (Stec -> m Stec) ->
    -- | current machine
    Stec ->
    -- | result 
    m Stec
seqStep k stec = case steccode of
    {- (code, environemnt, stack) -}
    SeqInstr instr : c -> case (instr, stecenv, stecstack) of
        {- Store:c, e, v:s ---> c, v:e, s -}
        (IStore, e, v:s) -> pure $
            stec & code !~ c
                 & environment %!~ cons v
        {- Access(n):c, e, s ---> c, e, e[n]:s -}
        (IAccess n, e, s) -> pure $
            stec & code !~ c
                 & stack %!~ cons (e !! n)
        {- Call(c'):c, e, s --> c', e, clos(c,e):s -}
        (ICall c', e, s) -> pure $
            stec & code !~ c'
                 & stack %!~ cons (VClos c e)

        {- Ret:c, e, v:clos(c',e'):s ---> c', e', v:s -}
        (IRet, e , v : VClos c' e': s) -> pure $
            stec & code !~ c'
                 & environment !~ e'
                 & stack %!~ cons v

        {- Cons(i,n):c, e, v_1:...:v_n:s --> c,e,cons(i,[v_1,...,v_n]):s-}
        (ICons i n, e, s) -> pure $
            stec & code !~ c
                 & stack !~ s'
                 & stack %!~ cons (VCons i args) 
          where
            (args, s') = splitAt n s

        {- Case(c_1,...,c_n):c, e: Cons(i,[v_1,...,v_n]):s ---> c_i, v_1:...:v_n:e, clo(c,e):s) -}
        (ICase cases, e, VCons i vs : s) -> pure $
            stec & code !~ (cases ! i)
                 & environment %!~ (vs++)
                 & stack %!~ cons (VClos c e) 

        {- Rec(c_1,...c_n):c, e, s ---> c, e, rec([c_1,..,c_n], e):s -}
        (IRec recs, e, s) -> pure $
            stec & code !~ c
                 & stack %!~ cons (VRec recs e) 

        {- Dest(i, n) : c, e , rec([c_1...c_n,e'):v_n:...:v_1:s ---> c_i, v_1...v_n:e', clo(c,e):s -}
        (IDest i n, e, VRec recs e' : s) -> pure $
            stec & code !~ (recs ! i)
                 & environment !~ e'
                 & environment %!~ (reverse args ++)
                 & stack %!~ cons (VClos c e)
          where
            (args, s') = splitAt n s

        {- Const(v) :c, e, s --> c, e, v : s -}
        (IConst v, e, s) -> pure $
            stec & code !~ c
                 & stack %!~ cons v

        {- built in operations -}
        (IAddInt, e, VInt n : VInt m : s) -> pure $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VInt $ m + n)
        (IMulInt, e, VInt n : VInt m : s) -> pure $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VInt $ m * n)
        (IEqInt, e, VInt n : VInt m : s) -> pure $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VBool $ m == n)
        (ILeqInt, e, VInt n : VInt m : s) -> pure $
            stec & code !~ c
                 & stack !~ s
                 & stack %!~ cons (VBool $ m <= n)
            
        _ -> k stec
    _ -> k stec
  where
    steccode = stec ^. code
    stecenv = stec ^. environment
    stecstack = stec ^. stack

concStep ::
    -- | continuation with what to do next if all pattern matches fails
    (Stec -> MplMach Stec) ->
    -- | current state to execute a step
    Stec ->
    MplMach Stec 
concStep k stec = case steccode of
    {- (stack, translation, environment, code -}
    ConcInstr instr : c -> case (stecstack, stectranslation, stecenv, instr) of
        (s,t,e,IGet ch) -> undefined
  where
    steccode = stec ^. code
    stecenv = stec ^. environment
    stecstack = stec ^. stack
    stectranslation = stec ^. translation
