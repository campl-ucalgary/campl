{-# LANGUAGE QuasiQuotes #-} module TypeInferenceSpec ( spec ) where

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

import Language.AbsMPL
import Language.LayoutMPL
import Language.ParMPL
import Language.ErrM

spec :: Spec
spec = do
    describe "TODO"  $ do
        it "TODO" $ do
            assertEqual "" True True

{-
translateParseLexGraph :: 
    String -> 
    Either ToGraphErrors (Prog (DefnG TaggedBnfcIdent TypeTag))
translateParseLexGraph str = progInterfaceToGraph $ unsafeTranslateParseLex str 


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
-}
