{-# LANGUAGE ScopedTypeVariables #-}
module AMPLConcurrentSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import AMPLConcurrent
import AMPLTypes

import Data.List
import Data.Tuple
import Control.Arrow

import Data.Map ( Map )
import qualified Data.Map as Map 

import Data.Set ( Set )
import qualified Data.Set as Set 

import Data.Queue ( Queue , (<|))
import qualified Data.Queue as Queue


spec :: Spec
spec = do
    describe "Step concurrent examples..." $ do
        it "TODO -- no step concurrent test cases written yet!" $ do
            assertEqual "" True True

    describe "Step channel manager examples..." $ do
        it "Id step with idTest0 and idTest0Res" $ do
            channelStepTester 
                idTest0
                idTest0Res
                (StepChannelManagerResult [] [] [] [(GlobalChanID 1, GlobalChanID 0)])
        it "Id step with idTest0Swapped and idTest0ResSwapped" $ do
            channelStepTester 
                idTest0Swapped
                idTest0ResSwapped
                (StepChannelManagerResult [] [] [] [(GlobalChanID 1, GlobalChanID 0)])

        it "Id step with idTestNothing0 and idTestNothing0Res" $ do
            channelStepTester 
                idTestNothing0
                idTestNothing0Res
                (StepChannelManagerResult [] [] [] []) 

        it "Id step with idTestNothing0Swapped and idTestNothing0ResSwapped" $ do
            channelStepTester 
                idTestNothing0Swapped
                idTestNothing0ResSwapped
                (StepChannelManagerResult [] [] [] [])

        it "Id step with idTestNothing1 and idTestNothing1Res" $ do
            channelStepTester 
                idTestNothing1
                idTestNothing1Res
                (StepChannelManagerResult [] [] [] [])

        it "Id step with idTestNothing1Swapped and idTestNothing1ResSwapped" $ do
            channelStepTester 
                idTestNothing1Swapped
                idTestNothing1ResSwapped
                (StepChannelManagerResult [] [] [] [])

        it "Id step with memory cell issue" $ do
            channelStepTester 
                memoryCellBadIdChm
                memoryCellBadIdChmRes
                (StepChannelManagerResult [] [] [] [(GlobalChanID 1, GlobalChanID 3)])


channelStepTester lstchm lstchmres res = do
    let chm = Map.fromList lstchm
        (res', chm') = stepChannelManager Set.empty chm
    assertBool ("Stepped channel manager is an invalid map. \n We stepped: \n" 
        ++ intercalate "\n" (map show (Map.toList chm)) 
        ++ "\nBut got: \n"
        ++ intercalate "\n" (map show (Map.toList chm'))) 
        (Map.valid chm')
    assertEqual "Stepped channel manager:" (Map.fromList lstchmres) chm'
    assertEqual "StepChannelManagerResult:" res res' 

-- Note: these are technically not valid machine states, but we want to use
-- invalid machine states just so we are testing the actions of Id -- in
-- case the result will lead to another step (e.g. a get/put step) which will
-- occur depending on the iteration order...

-- These tests test if id will do nothing given nothing to do
idTestNothing0 =
    [ (GlobalChanID 0, ( QGet mempty <| Queue.empty, QId (GlobalChanID 0, GlobalChanID 1) <| Queue.empty))
    , (GlobalChanID 1, ( QGet mempty <| Queue.empty, QGet mempty <| Queue.empty)) 
    ]
idTestNothing0Res = idTestNothing0

idTestNothing0Swapped = map (second swap) idTestNothing0
idTestNothing0ResSwapped = idTestNothing0Swapped

idTestNothing1 =
    [ (GlobalChanID 0, ( QGet mempty <| Queue.empty, QId (GlobalChanID 0, GlobalChanID (-1)) <| Queue.empty))
    , (GlobalChanID (-1), ( QGet mempty <| Queue.empty, QGet mempty <| Queue.empty)) 
    ]
idTestNothing1Res = idTestNothing1

idTestNothing1Swapped = map (second swap) idTestNothing1
idTestNothing1ResSwapped = idTestNothing1Swapped

-- These tests test if id will do nothing given nothing to do
idTest0 =
    [ (GlobalChanID 0, ( QGet mempty <| Queue.empty, QId (GlobalChanID 0, GlobalChanID 1) <| Queue.empty))
    , (GlobalChanID 1, ( Queue.empty, QGet mempty <| Queue.empty)) 
    ]
idTest0Res = 
    [ (GlobalChanID 0, ( QGet mempty <| Queue.empty, QGet mempty <| Queue.empty)) ]

idTest0Swapped = map (second swap) idTest0
idTest0ResSwapped = map (second swap) idTest0Res


-- Real world examples..
memoryCellBadIdChm = map (second (Queue.fromList *** Queue.fromList))
    [ (GlobalChanID (-1), ([], []))
    , (GlobalChanID 0, ([], []))
    , (GlobalChanID 1, ([], []))
    , (GlobalChanID 2, ([], []))
    , (GlobalChanID 3,
     ([QHPut (HCaseIx 0),
       QGet ([],
             [(Output, (LocalChanID 2, GlobalChanID 3)),
              (Output, (LocalChanID 3, GlobalChanID 4)),
              (Output, (LocalChanID 0, GlobalChanID 2)),
              (Output, (LocalChanID 1, GlobalChanID (-1)))],
             [],
             [SequentialInstr IStore,
              ConcurrentInstr (IHPut (LocalChanID 1) (HCaseIx 1)),
              SequentialInstr (IAccess 0),ConcurrentInstr (IPut (LocalChanID 1)),
              ConcurrentInstr (IHPut (LocalChanID 1) (HCaseIx 0)),
              ConcurrentInstr (IGet (LocalChanID 1)),SequentialInstr IStore,
              ConcurrentInstr (IHPut (LocalChanID 2) (HCaseIx 1)),
              SequentialInstr (IAccess 0),ConcurrentInstr (IPut (LocalChanID 2)),
              ConcurrentInstr (IFork (LocalChanID 3)
                                     ((LocalChanID 4,
                                       [LocalChanID 2],
                                       [ConcurrentInstr (IId (LocalChanID 4) (LocalChanID 2))]),
                                      (LocalChanID 5,
                                       [LocalChanID 1],
                                       [ConcurrentInstr (IRun [(Output, (LocalChanID 0, LocalChanID 5)),
                                                               (Output, (LocalChanID 1, LocalChanID 1))]
                                                              (FunID 1)
                                                              0)])))])],
      [QId (GlobalChanID 3, GlobalChanID 1)]))
    , (GlobalChanID 4, ([], [QSplit (GlobalChanID 5, GlobalChanID 6)]))
    , (GlobalChanID 5, ([], []))
    , (GlobalChanID 6, ([], [])) ]
memoryCellBadIdChmRes = map (second (Queue.fromList *** Queue.fromList))
    [ (GlobalChanID (-1), ([], []))
    , (GlobalChanID 0, ([], []))
    , (GlobalChanID 2, ([], []))
    , (GlobalChanID 3,
     ([QHPut (HCaseIx 0),
       QGet ([],
             [(Output, (LocalChanID 2, GlobalChanID 3)),
              (Output, (LocalChanID 3, GlobalChanID 4)),
              (Output, (LocalChanID 0, GlobalChanID 2)),
              (Output, (LocalChanID 1, GlobalChanID (-1)))],
             [],
             [SequentialInstr IStore,
              ConcurrentInstr (IHPut (LocalChanID 1) (HCaseIx 1)),
              SequentialInstr (IAccess 0),ConcurrentInstr (IPut (LocalChanID 1)),
              ConcurrentInstr (IHPut (LocalChanID 1) (HCaseIx 0)),
              ConcurrentInstr (IGet (LocalChanID 1)),SequentialInstr IStore,
              ConcurrentInstr (IHPut (LocalChanID 2) (HCaseIx 1)),
              SequentialInstr (IAccess 0),ConcurrentInstr (IPut (LocalChanID 2)),
              ConcurrentInstr (IFork (LocalChanID 3)
                                     ((LocalChanID 4,
                                       [LocalChanID 2],
                                       [ConcurrentInstr (IId (LocalChanID 4) (LocalChanID 2))]),
                                      (LocalChanID 5,
                                       [LocalChanID 1],
                                       [ConcurrentInstr (IRun [(Output, (LocalChanID 0, LocalChanID 5)),
                                                               (Output, (LocalChanID 1, LocalChanID 1))]
                                                              (FunID 1)
                                                              0)])))])],
      []))
    , (GlobalChanID 4, ([], [QSplit (GlobalChanID 5, GlobalChanID 6)]))
    , (GlobalChanID 5, ([], []))
    , (GlobalChanID 6, ([], [])) ]




{-
(GlobalChanID (-1), ([], []))
(GlobalChanID 0, ([], []))
(GlobalChanID 1, ([], []))
(GlobalChanID 2, ([], []))
(GlobalChanID 3,
 ([QHPut (HCaseIx 0),
   QGet ([],
         [(Output, (LocalChanID 2, GlobalChanID 3)),
          (Output, (LocalChanID 3, GlobalChanID 4)),
          (Output, (LocalChanID 0, GlobalChanID 2)),
          (Output, (LocalChanID 1, GlobalChanID (-1)))],
         [],
         [SequentialInstr IStore,
          ConcurrentInstr (IHPut (LocalChanID 1) (HCaseIx 1)),
          SequentialInstr (IAccess 0),ConcurrentInstr (IPut (LocalChanID 1)),
          ConcurrentInstr (IHPut (LocalChanID 1) (HCaseIx 0)),
          ConcurrentInstr (IGet (LocalChanID 1)),SequentialInstr IStore,
          ConcurrentInstr (IHPut (LocalChanID 2) (HCaseIx 1)),
          SequentialInstr (IAccess 0),ConcurrentInstr (IPut (LocalChanID 2)),
          ConcurrentInstr (IFork (LocalChanID 3)
                                 ((LocalChanID 4,
                                   [LocalChanID 2],
                                   [ConcurrentInstr (IId (LocalChanID 4) (LocalChanID 2))]),
                                  (LocalChanID 5,
                                   [LocalChanID 1],
                                   [ConcurrentInstr (IRun [(Output, (LocalChanID 0, LocalChanID 5)),
                                                           (Output, (LocalChanID 1, LocalChanID 1))]
                                                          (FunID 1)
                                                          0)])))])],
  [QId (GlobalChanID 3, GlobalChanID 1)]))
(GlobalChanID 4, ([], [QSplit (GlobalChanID 5, GlobalChanID 6)]))
(GlobalChanID 5, ([], []))
(GlobalChanID 6, ([], []))
-}
