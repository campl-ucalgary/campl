{-# LANGUAGE QuasiQuotes #-}
module MplPasses.TypeChecker.NonTypeErrorsSpec ( spec ) where

import Optics

import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Text.RawString.QQ

import MplAST.MplCore
import MplPasses.Assertions
import MplPasses.PassesErrors

import Data.Maybe
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE

import Data.Either
import Data.Traversable

import qualified MplPasses.Parser.BnfcParse as B
import MplPasses.Parser.Parse
import MplPasses.Parser.ParseErrors
import MplPasses.Renamer.Rename
import MplPasses.Renamer.RenameErrors
import qualified MplPasses.Renamer.RenameSym as R

import MplPasses.TypeChecker.TypeCheck

import MplPasses.TypeChecker.KindCheck 
import MplPasses.TypeChecker.TypeEqns 
import MplPasses.TypeChecker.TypeCheckSemanticErrors 
import MplPasses.TypeChecker.TypeCheckCallErrors 
import MplPasses.TypeChecker.TypeCheckErrorPkg 
import MplPasses.TypeChecker.TypeCheckMplTypeSub
import MplPasses.TypeChecker.TypeCheckErrors 

import MplPasses.Passes
import MplPasses.Env

spec = do
    mapM_ (`describeAnyErrors` ("Illegal expression codata call but got data instead", 
            _MplTypeCheckErrors 
            % _CallErrors 
            % _IllegalExprCodataCallGotDataInstead)
            )
        [ n0
        ]

    mapM_ (`describeAnyErrors` ("Non exhaustive fork scope..", 
            _MplTypeCheckErrors 
            % _SemanticErrors 
            % _ForkHasChannelsInScopeButContextsAreNonExhaustiveWith)
            )
        [ n1
        ]

    mapM_ (`describeAnyErrors` ("Fork non disjoint channels", 
            _MplTypeCheckErrors 
            % _SemanticErrors 
            % _ForkExpectedDisjointChannelsButHasSharedChannels)
            )
        [ n2
        ]

    mapM_ (`describeAnyErrors` ("Illegal ID of channels with the same polarity", 
            _MplTypeCheckErrors 
            % _SemanticErrors 
            % _IllegalIdGotChannelsOfTheSamePolarityButIdNeedsDifferentPolarity
            )
            )
        [ n3
        ]

    mapM_ (`describeAnyErrors` ("Illegal IdNeg of channels with the same polarity", 
            _MplTypeCheckErrors 
            % _SemanticErrors 
            % _IllegalIdNegGotChannelsOfDifferentPolarityButIdNegNeedsTheSamePolarity
            )
            )
        [ n4
        ]

    mapM_ (`describeAnyErrors` ("Cut condition cycles", 
            _MplTypeCheckErrors 
            % _SemanticErrors 
            % _IllegalCycleInPlugPhrase
            )
            )
        [ nc0
        , nc1
        ]
    mapM_ (`describeAnyErrors` ("Cut condition unreachable", 
            _MplTypeCheckErrors 
            % _SemanticErrors 
            % _UnreachablePhrasesInPlugPhrase
            )
            )
        {- some legacy misunderstandings here, this test case is actualyl bad -}
        [ -- nu0
        ]


 -- IllegalIdNegGotChannelsOfDifferentPolarityButIdNegNeedsTheSamePolarity

-- bad codata call
n0 = [r|
data Nat -> S =
    Succ :: S -> S
    Zero ::   -> S

fun myConst =
    a -> (Succ := b -> b)
|]

-- fork hs channels in scope
n1 = [r|
proc n1 :: | => TopBot (*) TopBot, TopBot =
    | => a,other -> do
        fork a as
            s -> do
                halt s
            t -> do
                halt t
|]

-- fork disjoint
n2 = [r|
proc n2 :: | => TopBot (*) TopBot, TopBot =
    | => a,other -> do
        fork a as
            s -> do
                close other
                halt s
            t -> do
                close other
                halt t
|]

-- Id polarity error 
n3 = [r|
proc n3 =
    | a,b => -> a |=| b
|]

-- Idneg polarity error 
n4 = [r|
proc n4 =
    | a => b -> a |=| neg b
|]

-------
-- cut condition failures
nc0 = [r|
proc nc0 =
    |  => -> do
        plug
            a => b -> do
                close a
                halt b
            b => a -> do
                close a
                halt b
|]

nc1 = [r|
proc nc1 =
    |  b => g -> do
        plug
            a,b => c -> do
                close a
                close b 
                halt c 
            c => d,e -> do
                close c 
                close d
                halt e
            d => g -> do
                close d
                halt g
            e => a -> do
                close a
                halt e
|]

nu0 = [r|
proc nu0 =
    |  => -> do
        plug
            a,b =>  -> do
                close a 
                halt b
            => c,d -> do
                close c
                halt d
|]
