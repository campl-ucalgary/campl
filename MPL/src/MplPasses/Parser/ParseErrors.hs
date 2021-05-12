{-# LANGUAGE TemplateHaskell #-}
module MplPasses.Parser.ParseErrors where

import Optics

import Data.Function
import Data.Tuple

import MplAST.MplCore
import MplAST.MplParsed

import Data.List
import Data.List.NonEmpty (NonEmpty (..))

import MplPasses.PassesErrorsPprint

import Data.Foldable

import Data.Proxy

{- Module for defining possible parse errors -}

data ParseErrors 
    = ExpectedGetOrPutButGot IdentP
    | InvalidInt IdentP
    | InvalidDouble IdentP
    | InvalidChar IdentP
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

    | NegExpectedExactlyOneArgumentButGot 
        NameOcc [MplType MplParsed]

    | PlugExpectedTwoOrMorePhrasesButGot 
        (Maybe (CPlugPhrase MplParsed))
    | PlugExpectedARunProcessCallButGot 
        (NonEmpty (MplCmd MplParsed))
  deriving Show

$(makeClassyPrisms ''ParseErrors)

pprintParseErrors :: ParseErrors -> MplDoc
pprintParseErrors = go
  where
    go err = case err of
        ExpectedGetOrPutButGot idp -> hsep
            [ pretty "Expected"
            , backticksinglequote $ pretty "get"
            , pretty "or"
            , backticksinglequote $ pretty "put"
            , pretty "but instead got"
            , pprintIdentPWithLoc idp
            ]
        InvalidInt idp -> fold 
            [ pretty "Invalid integer (most likely out of range) with"
            , codeblock $ idp ^. identPNameOcc % nameOccName % (coerced :: Iso' Name String)
            , pretty "at" <+> pprintIdentPLoc idp
            ]
        InvalidDouble idp -> fold 
            [ pretty "Invalid double (most likely out of range) with"
            , codeblock $ idp ^. identPNameOcc % nameOccName % (coerced :: Iso' Name String)
            , pretty "at" <+> pprintIdentPLoc idp
            ]
        InvalidChar idp -> fold 
            [ pretty "Invalid char with"
            , codeblock $ idp ^. identPNameOcc % nameOccName % (coerced :: Iso' Name String)
            , pretty "at" <+> pprintIdentPLoc idp
            ]
        ExpectedTypeVarButGot tp -> fold
            [ pretty "Expected a type variable (upper case identifier) but instead got"
            , codeblock $ pprintParsed tp
            , pretty "at" <+> pprintSpan (typeLocationSpan tp)
            ]
        ExpectedTypeSeqWithArgsButGot tp -> fold
            [ pretty "Expected a sequential type with arguments but got"
            , codeblock $ pprintParsed tp
            , pretty "at" <+> pprintSpan (typeLocationSpan tp)
            ]
        ExpectedTypeConcWithArgsButGot tp -> fold
            [ pretty "Expected a concurrent type with arguments but got"
            , codeblock $ pprintParsed tp
            , pretty "at" <+> pprintSpan (typeLocationSpan tp)
            ]
        ExpectedCodataPhraseToHaveFromArgsButHasNone identps -> fold
            [ pretty "Expected the following codata phrases to have arguments on the left side of the arrow: "
            , hsep $ map pprintIdentPWithLoc identps
            ]
        CaseExpectedExactlyOnePatternButGot patterns -> fold
            [ pretty "Expected `case' to have exactly one pattern by got" 
            , codeblock $ intercalate "," $ map pprintParsed patterns
            , pretty "TODO (for developers) compute the span of the patterns"
            ]
        SplitExpectedExactlyTwoChannelsButGot chs -> fold
            [ pretty "Expected `split' to have exactly two channels but got" 
            , hsep $ map pprintIdentPWithLoc chs
            ]
        ForkExpectedExactlyTwoForkedChannelsButGot _ -> fold
            [ pretty "Expected `fork' to have exactly two channels"]
        {- ForkExpectedExactlyTwoForkedChannelsButGot 
            [(IdentP, Maybe [IdentP], NonEmpty (MplCmd MplParsed))]
        -}
        NegExpectedExactlyOneArgumentButGot neg args -> fold
            [ pretty "Expected `Neg' to have exactly one argument"]
        {- NegExpectedExactlyOneArgumentButGot NameOcc [MplType MplParsed] -}

        PlugExpectedTwoOrMorePhrasesButGot phrases -> fold
            [ pretty "Expected `plug' to have two or more phrases"]
        {- PlugExpectedTwoOrMorePhrasesButGot (Maybe (CPlugPhrase MplParsed)) -}

        PlugExpectedARunProcessCallButGot procscall -> fold
            [ pretty "Expected `plug' to call a process" ]
        {- PlugExpectedARunProcessCallButGot (NonEmpty (MplCmd MplParsed)) -}

