{-# LANGUAGE QuasiQuotes #-}
module TypeInferenceSpec ( spec ) where

import Test.Hspec
import Test.QuickCheck
import Test.HUnit
import Text.RawString.QQ

import Data.Maybe
import Data.List.NonEmpty ( NonEmpty (..) )
import qualified Data.List.NonEmpty as NE

import MPLAST.MPLASTTranslate
import MPLAST.MPLASTTranslateErrors
import MPLAST.MPLProgI
import MPLAST.MPLASTCore
import MPLCompile

import MPLPasses.ToGraph
import MPLPasses.ToGraphErrors

import Language.AbsMPL
import Language.LayoutMPL
import Language.ParMPL
import Language.ErrM

translateParseLexGraph :: 
    String -> 
    Either ToGraphErrors (Prog (DefnG TaggedBnfcIdent TypeTag))
translateParseLexGraph str = progInterfaceToGraph $ unsafeTranslateParseLex str 

spec :: Spec
spec = do
    describe "hmmm"  $ do
        it "Exhaustively testing equalities.." $ do
            assertEqual "" True True

natfun = [r|
defn 
    fun natfun =
        Nat(Nat(a)),Zero -> Zero
where
    data
        Nat -> STATEVAR =
            Nat :: STATEVAR -> STATEVAR
            Zero ::         -> STATEVAR
|]
