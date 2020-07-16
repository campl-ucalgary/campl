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
import MPLAST.MPLProgGraph
import MPLAST.MPLTypeAST
import MPLAST.MPLASTCore

import Data.Maybe
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE

-- helper functions for running the graph maker..
emptyContext = TieTypeClauseContext [] (UniqueTag 0)
unsafeRunMakeTypeClauseGraph :: NonEmpty (TypeClause () () () BnfcIdent BnfcIdent) -> ClausesGraph TaggedBnfcIdent
unsafeRunMakeTypeClauseGraph a = case makeTypeClauseGraph DataObj emptyContext a of
        Right n -> snd n
        Left (n :: NonEmpty TieTypeClauseError) -> error $ show n 

unsafeMakeTypeClauseGraphFromStr str = case unsafeTranslateParseLex str of
    Prog [Stmt (DefnI (DataDefn n):| []) _] -> unsafeRunMakeTypeClauseGraph n

spec :: Spec
spec = do
    describe "Tie type clause wth hand written example.."  $ do
        it "Exhaustively testing equalities.." $ do
            let spine = (unsafeRunMakeTypeClauseGraph mutuallyrecursiveTest) ^. clauseGraphSpine
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
            -- clause2Test $ fromJust (((NE.toList spine !! 0 ^. typeClausePhrases) !! 0) ^? typePhraseTo 
            --                 % _TypeWithArgs % _2 % _TypeClauseNode)
            -- clause1Test $ fromJust (((NE.toList spine !! 1 ^. typeClausePhrases) !! 0) ^? typePhraseTo 
            --                 % _TypeWithArgs % _2 % _TypeClauseNode)



    -- , _typeClauseStateVar ::  ident
    -- , _typeClausePhrases :: [TypePhrase phrasecontext calldef ident]
    -- , _typeClauseNeighbors :: neighbors

    -- _typePhraseContext :: phrasecontext
    -- , _typePhraseName :: ident
    -- , _typePhraseFrom :: [Type calldef ident]
    -- , _typePhraseTo :: Type calldef ident
                    



{-
makeTypeClauseGraph :: 
    -- AsTieTypeClauseError e =>
    ObjectType -> 
    TieTypeClauseContext -> 
    NonEmpty (TypeClause () () () BnfcIdent) -> 
    -- Either e (UniqueTag, ClausesGraph TypeClauseNode TaggedBnfcIdent)
    Either TieTypeClauseError (UniqueTag, ClausesGraph TypeClauseNode TaggedBnfcIdent)
    -}

testClauses' = unsafeMakeTypeClauseGraphFromStr testClauses
testClauses = [r|
data
    Clause1 -> A =
        Phrase1 :: -> B
    and
    Clause2 -> B =
        Phrase2 :: -> A
|]

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
