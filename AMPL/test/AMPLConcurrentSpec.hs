{-# LANGUAGE ScopedTypeVariables #-}
module AMPLConcurrentSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import AMPLConcurrent
import AMPLTypes

import Data.Tuple
import Control.Arrow

import Data.Map ( Map )
import qualified Data.Map as Map 

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
                (StepChannelManagerResult [] [GlobalChanID 0] [])

        it "Id step with idTest0Swapped and idTest0ResSwapped" $ do
            channelStepTester 
                idTest0Swapped 
                idTest0ResSwapped 
                (StepChannelManagerResult [] [GlobalChanID 0] [])

        it "Id step with idTest1 and idTest1Res" $ do
            channelStepTester 
                idTest1 
                idTest1Res
                (StepChannelManagerResult [] [GlobalChanID 0] [])

        it "Id step with idTest1Swapped and idTest1ResSwapped" $ do
            channelStepTester 
                idTest1Swapped 
                idTest1ResSwapped
                (StepChannelManagerResult [] [GlobalChanID 0] [])

        it "Id step with idTestNothing0 and idTestNothing0Res" $ do
            channelStepTester 
                idTestNothing0
                idTestNothing0Res
                (StepChannelManagerResult [] [] [])

        it "Id step with idTestNothing0Swapped and idTestNothing0ResSwapped" $ do
            channelStepTester 
                idTestNothing0Swapped
                idTestNothing0ResSwapped
                (StepChannelManagerResult [] [] [])

        it "Id step with idTestNothing1 and idTestNothing1Res" $ do
            channelStepTester 
                idTestNothing1
                idTestNothing1Res
                (StepChannelManagerResult [] [] [])

        it "Id step with idTestNothing1Swapped and idTestNothing1ResSwapped" $ do
            channelStepTester 
                idTestNothing1Swapped
                idTestNothing1ResSwapped
                (StepChannelManagerResult [] [] [])

channelStepTester lstchm lstchmres res = do
    let chm = Map.fromList lstchm
        (res', chm') = stepChannelManager chm
    assertBool "Stepped channel manager is a valid map" (Map.valid chm')
    assertEqual "Stepped channel manager:" (Map.fromList lstchmres) chm'
    assertEqual "StepChannelManagerResult:" res res' 

-- Note: these are technically not valid machine states, but we want to use
-- invalid machine states just so we are testing the actions of Id -- in
-- case the result will lead to another step (e.g. a get/put step) which will
-- occur depending on the iteration order...
idTest0 = 
    [ (GlobalChanID 0, ( QGet mempty <| Queue.empty, QId (GlobalChanID 0, GlobalChanID 1) <| Queue.empty))
    , (GlobalChanID 1, ( Queue.empty, QGet mempty <| Queue.empty)) 
    ]
idTest0Res = 
    [ (GlobalChanID 1, (QGet mempty <| Queue.empty, QGet mempty <| Queue.empty)) ]

idTest0Swapped = map (second swap) idTest0
idTest0ResSwapped = map (second swap) idTest0Res

idTest1 = 
    [ (GlobalChanID 0, ( QGet mempty <| Queue.empty, QId (GlobalChanID 0, GlobalChanID (-1)) <| Queue.empty))
    , (GlobalChanID (-1), ( Queue.empty, QGet mempty <| Queue.empty)) 
    ]
idTest1Res = 
    [ (GlobalChanID (-1), (QGet mempty <| Queue.empty, QGet mempty <| Queue.empty)) ]

idTest1Swapped = map (second swap) idTest1
idTest1ResSwapped = map (second swap) idTest1Res

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
