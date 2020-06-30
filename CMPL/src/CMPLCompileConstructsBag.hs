{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module CMPLCompileConstructsBag where

import CMPLAST
import CMPLCompileAST
import CMPLConstructsBag

import AMPLAST (ACom (..), AComIdents (..))
import AMPLUntaggedConstructBag
import qualified AMPLAST as AMPL

import Data.Functor.Foldable

import Control.Monad.State
import Control.Monad.Reader

import Data.Stream (Stream)
import qualified Data.Stream as Stream

import Optics.TH
import Optics.Optic
import Optics.State.Operators
import Optics.Operators
import Optics.Iso
import Optics.Re
import Optics.View
import Optics.Setter
import Optics.Traversal
import Optics.Fold
import Data.Tuple.Optics
import Data.Tuple

import Control.Arrow

import Control.Monad.State

cmplCompileConstructsBag :: CmplConstructsBag -> UntaggedAmplAsmBag 
cmplCompileConstructsBag cmplBag = UntaggedAmplAsmBag {
        untaggedAmplImports = cmplBag ^. cmplIncludes
        , untaggedAmplMainInfo = Just 
            ( cmplBag ^. cmplMainRun % mainInputChs
            , cmplBag ^. cmplMainRun % mainOutputChs
            , maincmds )
        , untaggedAmplConstructsBag = 
            UntaggedAmplConstructsBag {
                untaggedProtocolInfo = protocolCoprotocolHelper 
                    $ cmplBag ^. cmplProtocols
                , untaggedCoprotocolInfo = protocolCoprotocolHelper 
                    $ cmplBag ^. cmplCoprotocols
                , untaggedDataInfo = dataCodataHelper 
                    $ cmplBag ^. cmplData
                , untaggedCodataInfo = dataCodataHelper 
                    $ cmplBag ^. cmplCodata
                , untaggedProcessInfo = processHelper
                    $ cmplBag ^. cmplProcess
                , untaggedFunctionInfo = functionHelper
                    $ cmplBag ^. cmplFunction
            }
        }
  where
    runProcessCommandCompiler n = evalState 
        (cmplCompileProcessCommand n) defaultCmplUniqueVarState 

    runExprCompiler n = evalState 
        (cmplCompileExpr n) defaultCmplUniqueVarState 

    maincmds = runProcessCommandCompiler (cmplBag ^. cmplMainRun % mainProcessCommands)

    protocolCoprotocolHelper =
        map ((^.protocolCoprotocolName) &&& (^.protocolCoprotocolSubclauses) ) 

    dataCodataHelper = 
        map ((^.dataCodataName) &&& (^.dataCodataSubclauses) )

    processHelper = 
        map (\n -> 
                ( n^. processName
                , ( n ^. processSeqArgs
                    , n ^. processInputChs
                    , n ^. processOutputChs
                    , runProcessCommandCompiler (n ^. processCommands)
                    ) )
                )
    
    functionHelper =
        map (\n -> 
                ( n ^. functionName
                , (n ^. functionArgs
                    , runExprCompiler (n ^. functionExpr) ++ [ARet ()]
                    -- need to tag the return at the end
                    )
                )
            )
