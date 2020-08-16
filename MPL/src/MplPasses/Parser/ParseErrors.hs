{-# LANGUAGE TemplateHaskell #-}
module MplPasses.Parser.ParseErrors where

import Optics

import Data.Function
import Data.Tuple

import MplAST.MplCore
import MplAST.MplParsed

import Data.List.NonEmpty (NonEmpty (..))

data ParseErrors =
    ExpectedGetOrPutButGot IdentP
    | InvalidInt IdentP
    | ExpectedTypeVarButGot (MplType MplParsed)
    | ExpectedTypeSeqWithArgsButGot (MplType MplParsed)
    | ExpectedTypeConcWithArgsButGot (MplType MplParsed)

    -- handle /phrase names
    | ExpectedCodataPhraseToHaveFromArgsButHasNone [IdentP] 

    | CaseExpectedExactlyOnePatternButGot 
        [MplPattern MplParsed]

    | SplitExpectedExactlyTwoChannelsButGot [IdentP]
    | ForkExpectedExactlyTwoForkedChannelsButGot 
        [(IdentP, Maybe [IdentP], NonEmpty (MplCmd MplParsed))]

    | PlugExpectedTwoOrMorePhrasesButGot 
        (Maybe (Maybe [IdentP], NonEmpty (MplCmd MplParsed)))
  deriving Show

$(makeClassyPrisms ''ParseErrors)
