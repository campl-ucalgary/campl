{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MPLPasses.TieTypeClauseSpec ( spec ) where

import Optics

import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Text.RawString.QQ

import MPLCompile
import MPLPasses.TieTypeClause
import MPLPasses.GraphGenCore
import MPLPasses.TieDefnsErrors
import MPLAST.MPLProg
import MPLAST.MPLProgGraph
import MPLAST.MPLTypeAST
import MPLAST.MPLASTCore

import Data.Maybe
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE
import Control.Monad.RWS

import System.IO.Unsafe

-- helper functions for running the graph maker..
{-# NOINLINE testGraphGenCoreState #-}
testGraphGenCoreState = unsafePerformIO defaultGraphGenCoreState

runMakeTypeClauseGraph :: NonEmpty (TypeClause () () () BnfcIdent BnfcIdent) -> Either [TieDefnsError] (ClausesGraph TaggedBnfcIdent) 
runMakeTypeClauseGraph a = 
    case runRWS (unGraphGenCore (tieTypeClauseGraph [] DataObj a))
        defaultGraphGenCoreEnv testGraphGenCoreState of
        (res,_, []) -> Right res
        (_ ,_,errs) -> Left errs

unsafeMakeTypeClauseGraphFromStr str = case unsafeTranslateParseLex str of
    Prog [Stmt (DefnI (DataDefn n):| []) _] -> runMakeTypeClauseGraph n

spec :: Spec
spec = do
    describe "Tie type clause wth hand written example.."  $ do
        it "Valid mutually recursive data type -- exhaustively testing equalities on the correctness of tying the knot.." $ do
            let Right typeclauseg = runMakeTypeClauseGraph mutuallyrecursiveTest
                spine = typeclauseg ^. clauseGraphSpine
                spineTest tspine = do
                    assertEqual "" 2 (length spine) 

                    clause1Test (NE.toList tspine !! 0)
                    clause2Test (NE.toList tspine !! 1)

                clause1Test tspine = do
                    assertEqual "" "Clause1" (tspine ^. typeClauseName % taggedBnfcIdentName) 
                    assertEqual "" [] (tspine ^. typeClauseArgs) 
                    assertEqual "" "ST" (tspine ^. typeClauseStateVar % taggedBnfcIdentName) 
                    assertEqual "" 1 (length (tspine ^. typeClausePhrases)) 
                    assertEqual "" "Phrase1" ((tspine ^. typeClausePhrases) 
                                                !! 0 ^. typePhraseName % taggedBnfcIdentName)
                clause2Test tspine = do
                    assertEqual "" "Clause2" (tspine ^. typeClauseName % taggedBnfcIdentName) 
                    assertEqual "" [] (tspine ^. typeClauseArgs) 
                    assertEqual "" "Potato" (tspine ^. typeClauseStateVar % taggedBnfcIdentName) 
                    assertEqual "" 1 (length (tspine ^. typeClausePhrases)) 
                    assertEqual "" "Phrase2" ((tspine ^. typeClausePhrases) 
                                                !! 0 ^. typePhraseName % taggedBnfcIdentName) 

            -- testing if the spine is the same between neighbors
            spineTest spine
            spineTest (NE.toList spine !! 0 ^. typeClauseNeighbors % clauseGraph % clauseGraphSpine)
            spineTest (NE.toList spine !! 1 ^. typeClauseNeighbors % clauseGraph % clauseGraphSpine)

            -- testing the parent child relatioship..
            clause1Test ((NE.toList spine !! 0 ^. typeClausePhrases) !! 0 ^. typePhraseContext % phraseParent)
            clause2Test ((NE.toList spine !! 1 ^. typeClausePhrases) !! 0 ^. typePhraseContext % phraseParent)

            -- testing if the substituted variables really are substituted to the correct parts 
            -- we no longer immediately substitute the state variables when making the graph
            -- So, we do not test this...
            -- clause2Test $ fromJust (((NE.toList spine !! 0 ^. typeClausePhrases) !! 0) ^? typePhraseTo 
            --                 % _TypeWithArgs % _2 % _TypeClauseNode)
            -- clause1Test $ fromJust (((NE.toList spine !! 1 ^. typeClausePhrases) !! 0) ^? typePhraseTo 
            --                 % _TypeWithArgs % _2 % _TypeClauseNode)
        it "Testing type out of scope..." $ do
            let typeclauseg = runMakeTypeClauseGraph outofscopeTest
            assertBool ("Expected a (Left out of scope), but got: " ++ show typeclauseg) 
                (has (_Left % traversed % _NotInScope ) typeclauseg)
            


mutuallyrecursiveTest = NE.fromList [
    TypeClause (mkident "Clause1") [] (mkident "ST")
        [ TypePhrase ()
            (mkident "Phrase1")
            []
            (TypeWithArgs (mkident "Potato") () [])
        ]
        ()
    , TypeClause (mkident "Clause2") [] (mkident "Potato")
        [ TypePhrase ()
            (mkident "Phrase2")
            []
            (TypeWithArgs (mkident "ST") () [])
        ]
        ()
    ]
  where
    mkident str = (review _BnfcIdent (str, (1,1)))

outofscopeTest = NE.fromList [
    TypeClause (mkident "Clause1") [] (mkident "ST")
        [ TypePhrase ()
            (mkident "Phrase1")
            []
            (TypeWithArgs (mkident "Potato") () [])
        ]
        ()
    ]
  where
    mkident str = (review _BnfcIdent (str, (1,1)))
