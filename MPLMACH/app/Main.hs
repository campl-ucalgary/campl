module Main where

import MplMach.MplMachRunner
import MplMach.MplMachStep
import MplMach.MplMachTypes
import MplMach.MplMachStack

import Data.Array

main :: IO ()
main = do 
    env <- initMplMachEnv brokenSuperComb
    mplMachRunnner env brokenMain

{-# NOINLINE brokenSuperComb #-}
brokenSuperComb = 
    MpMachSuperCombinators
      { _supercombinators =
          array
            ( CallIx 0 , CallIx 2 )
            [ ( CallIx 0
              , [ SeqInstr (IAccess 0)
                , SeqInstr IStore
                , SeqInstr (IAccess 0)
                , SeqInstr (ITupleElem (TupleIx 0))
                , SeqInstr IRet
                ]
              )
            , ( CallIx 1
              , [ SeqInstr (IAccess 0)
                , SeqInstr (IAccess 1)
                , SeqInstr ILtInt
                , SeqInstr IStore
                , SeqInstr (IAccess 0)
                , SeqInstr
                    (IIf
                       [ SeqInstr (IConst (VInt 0))
                       , SeqInstr IStore
                       , SeqInstr (IAccess 3)
                       , SeqInstr IStore
                       , SeqInstr (IAccess 0)
                       , SeqInstr (IAccess 1)
                       , SeqInstr (ITuple 2)
                       , SeqInstr IRet
                       ]
                       [ SeqInstr (IConst (VInt 1))
                       , SeqInstr (IAccess 1)
                       , SeqInstr (IAccess 2)
                       , SeqInstr ISubInt
                       , SeqInstr IStore
                       , SeqInstr (IAccess 2)
                       , SeqInstr IStore
                       , SeqInstr (IAccess 1)
                       , SeqInstr IStore
                       , SeqInstr (IAccess 1)
                       , SeqInstr IStore
                       , SeqInstr (ICall (CallIx 1) 2)
                       , SeqInstr IStore
                       , SeqInstr (IAccess 0)
                       , SeqInstr (ITupleElem (TupleIx 0))
                       , SeqInstr IAddInt
                       , SeqInstr IStore
                       , SeqInstr (IAccess 5)
                       , SeqInstr (IAccess 6)
                       , SeqInstr ISubInt
                       , SeqInstr IStore
                       , SeqInstr (IAccess 6)
                       , SeqInstr IStore
                       , SeqInstr (IAccess 1)
                       , SeqInstr IStore
                       , SeqInstr (IAccess 1)
                       , SeqInstr IStore
                       , SeqInstr (ICall (CallIx 1) 2)
                       , SeqInstr IStore
                       , SeqInstr (IAccess 0)
                       , SeqInstr (ITupleElem (TupleIx 1))
                       , SeqInstr IStore
                       , SeqInstr (IAccess 0)
                       , SeqInstr (IAccess 4)
                       , SeqInstr (ITuple 2)
                       , SeqInstr IRet
                       ])
                , SeqInstr IRet
                ]
              )
            , ( CallIx 2
              , [ ConcInstr (ISHPut (LocalChan (-1)) SHPutInt)
                , SeqInstr (IConst (VInt 150))
                , SeqInstr IStore
                , SeqInstr (IConst (VInt 10))
                , SeqInstr IStore
                , SeqInstr (IAccess 1)
                , SeqInstr IStore
                , SeqInstr (IAccess 1)
                , SeqInstr IStore
                , SeqInstr (ICall (CallIx 1) 2)
                , SeqInstr IStore
                , SeqInstr (IAccess 0)
                , SeqInstr IStore
                , SeqInstr (ICall (CallIx 0) 1)
                , SeqInstr IStore
                , SeqInstr (IAccess 0)
                , ConcInstr (IPut (LocalChan (-1)))
                , ConcInstr (ISHPut (LocalChan (-1)) SHClose)
                , ConcInstr (IHalt (LocalChan (-1)))
                ]
              )
            ]
      }
{-# NOINLINE brokenMain #-}
brokenMain = 
    ( ( [] , [ LocalChan (-1) ] )
    , [ ConcInstr (ISHPut (LocalChan (-1)) SHPutInt)
      -- change this int to break things more or less
      , SeqInstr (IConst (VInt 50))
      , SeqInstr IStore
      , SeqInstr (IConst (VInt 10))
      , SeqInstr IStore
      , SeqInstr (IAccess 1)
      , SeqInstr IStore
      , SeqInstr (IAccess 1)
      , SeqInstr IStore
      , SeqInstr (ICall (CallIx 1) 2)
      , SeqInstr IStore
      , SeqInstr (IAccess 0)
      , SeqInstr IStore
      , SeqInstr (ICall (CallIx 0) 1)
      , SeqInstr IStore
      , SeqInstr (IAccess 0)
      , ConcInstr (IPut (LocalChan (-1)))
      , ConcInstr (ISHPut (LocalChan (-1)) SHClose)
      , ConcInstr (IHalt (LocalChan (-1)))
      ]
    )

