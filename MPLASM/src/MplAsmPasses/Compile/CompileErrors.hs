{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
module MplAsmPasses.Compile.CompileErrors where


import MplMach.MplMachTypes

import Optics
import MplAsmAST.MplAsmCore
import MplAsmPasses.PassesErrorsPprint

import Data.Traversable
import Data.Foldable

import Data.Text.Prettyprint.Doc

data CompileError x
    = OverlappingDeclarations [[IdP x]]
    | OutOfScopeVariable (IdP x)
    | OutOfScopeFun (IdP x)
    | OutOfScopeProc (IdP x)
    | OutOfScopeChannel (IdP x)

    | OutOfScopeProtocol (TypeAndSpec x)
    | OutOfScopeCoprotocol (TypeAndSpec x)

    | NotAllSameCase [[TypeAndSpec x]]
    | NotAllSameRecord [[TypeAndSpec x]]
    | NotAllSameHCase [[TypeAndSpec x]]

    -- | fun name, expectaed number of args, actual number of args
    | IllegalFunCall (IdP x) Int Int
    | IllegalProcCall (IdP x) (Int,Int,Int) (Int,Int,Int)

    | OutOfScopeData (TypeAndSpec x)
    | IllegalConstructorCall (TypeAndSpec x) Int Int

    | OutOfScopeCodata (TypeAndSpec x)
    | IllegalDestructorCall (TypeAndSpec x) Int Int

    | UnknownInputService (IdP x)
    | UnknownOutputService (IdP x)

    | NoMainFunction 

$(makeClassyPrisms ''CompileError)

deriving instance (Show (IdP x), Show (TypeAndSpec x)) => Show (CompileError x)

pprintCompileErrors ::
    ( Pretty (IdP x) 
    , Pretty (TypeAndSpec x) 
    ) => 
    [CompileError x] ->
    MplAsmDoc
pprintCompileErrors = vsep . map go
  where
    go = \case
        OverlappingDeclarations ids -> fold
            [ pretty "Overlapping declarations of:"
            , line
            , indent' $ vcat $ map pretty ids
            ]
        OutOfScopeVariable idp -> fold
            [ pretty "Out of scope variable:"
            , line
            , indent' $ pretty idp
            ]
        OutOfScopeFun idp -> fold
            [ pretty "Out of scope function:"
            , line
            , indent' $ pretty idp
            ]
        OutOfScopeProc idp -> fold
            [ pretty "Out of scope process:"
            , line
            , indent' $ pretty idp
            ]
        OutOfScopeChannel idp -> fold
            [ pretty "Out of scope channel:"
            , line
            , indent' $ pretty idp
            ]
        OutOfScopeProtocol tpspec -> fold
            [ pretty "Out of scope protocol:"
            , line
            , indent' $ pretty tpspec
            ]
        OutOfScopeCoprotocol tpspec -> fold
            [ pretty "Out of scope coprotocol:"
            , line
            , indent' $ pretty tpspec
            ]
        NotAllSameCase tpspecs -> fold
            [ pretty "Alternatives in `case' are from different data:"
            , line
            , indent' $ vsep $ map pretty tpspecs
            ]
        NotAllSameRecord tpspecs -> fold
            [ pretty "Alternatives in `record' are from different codata:"
            , line
            , indent' $ vsep $ map pretty tpspecs
            ]

        NotAllSameHCase tpspecs -> fold
            [ pretty "Alternatives in `hcase' are from different protocols or coprotocols:"
            , line
            , indent' $ vsep $ map pretty tpspecs
            ]

        IllegalFunCall idp expectednumargs actualargs -> fold
            [ pretty "Illegal function call:"
            , line
            , indent' $ pretty idp
            , pretty "Expected" 
                <+> pretty expectednumargs
                <+> pretty "arguments, but got"
                <+> pretty actualargs
                <+> pretty "arguments."
            ]
        IllegalProcCall idp expected actual -> fold
            [ pretty "Illegal process call:"
            , line
            , indent' $ pretty idp
            , pretty "Expected" 
                <+> pretty expected
                <> pretty "; but got"
                <+> pretty actual
                <> pretty "."
            ]
          where
            prettyTriplet (seqs, ins, outs) = 
                pretty seqs <+> pretty "sequential arguments, "
                <+> pretty ins <+> pretty "input channel arguments, and "
                <+> pretty outs <+> pretty "output channel arguments"


        OutOfScopeData tpspec -> fold
            [ pretty "Out of scope data:"
            , line
            , indent' $ pretty tpspec
            ]

        IllegalConstructorCall tpspec expected actual -> fold
            [ pretty "Illegal constructor call:"
            , line
            , indent' $ pretty tpspec
            , pretty "Expected" 
                <+> pretty expected
                <+> pretty "arguments, but got"
                <+> pretty actual
                <+> pretty "arguments."
            ]
        OutOfScopeCodata tpspec -> fold
            [ pretty "Out of scope codata:"
            , line
            , indent' $ pretty tpspec
            ]

        IllegalDestructorCall tpspec expected actual -> fold
            [ pretty "Illegal destructor call:"
            , line
            , indent' $ pretty tpspec
            , pretty "Expected" 
                <+> pretty expected
                <+> pretty "arguments, but got"
                <+> pretty actual
                <+> pretty "arguments."
            ]

        UnknownInputService idp -> fold
            [ pretty "Unknown input polarity service:"
            , line
            , indent' $ pretty idp
            ]
        UnknownOutputService idp -> fold
            [ pretty "Unknown output polarity service:"
            , line
            , indent' $ pretty idp
            ]

        NoMainFunction -> fold
            [ pretty "No `run' process found i.e., there is no main function."
            ]

    indent' = indent 4
