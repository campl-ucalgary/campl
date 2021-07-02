module MplMach.MplMachStepSpec (spec) where

import Test.Hspec
import MplMach.MplMachStep
import MplMach.MplMachTypes
import MplMach.MplMachStack

import Data.List 
import Data.Functor.Identity

{- | this is a list of the unfolded sequential steps -}
unfoldedSeqSteps :: 
    Stec ->
    MplMach MplMachSuperCombinators [Stec]
unfoldedSeqSteps stec = (stec:) <$> go stec
  where
    go stec = do
        res <- seqStep (const (error "bad test case")) stec 
        case res of 
            Just stec' -> (stec' :) <$> go stec'
            Nothing -> return []

spec :: Spec
spec = do
    describe "seqStep" $ do
        it "removes leading and trailing whitespace" $ do
            id "a" `shouldBe` "a"
            -- strip "\t  foo bar\n" `shouldBe` "foo bar"
    return ()
