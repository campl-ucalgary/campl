{-# LANGUAGE TemplateHaskell #-}
module MplParse.MplInfixity where

import Optics
import Optics.TH
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Precedence = Int

data Infixity
    = InfixL
    | InfixR
    | InfixE

data InfixMap = InfixMap 
    { _infixrOperators :: Map Precedence String
    , _infixlOperators :: Map Precedence String
    , _infixOperators :: Map Precedence String
    }
$(makeLenses ''InfixMap)

insertInfixrOp :: 
    Precedence ->
    String -> 
    InfixMap ->
    Maybe InfixMap
insertInfixrOp k op opmap = undefined $ 
    Map.insertLookupWithKey f k op 
        (opmap ^. infixrOperators)
  where
    f = undefined

