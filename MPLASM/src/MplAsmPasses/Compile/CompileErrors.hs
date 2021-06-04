{-# LANGUAGE TemplateHaskell #-}
module MplAsmPasses.Compile.CompileErrors where

import AMPL
import AMPLTypes

import Optics
import MplAsmAST.MplAsmCore

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
    | IllegalFunCall (IdP x) Word Word
    | IllegalProcCall (IdP x) (Word,Word,Word) (Word,Word,Word)

    | OutOfScopeData (TypeAndSpec x)
    | IllegalConstructorCall (TypeAndSpec x) Word Word

    | OutOfScopeCodata (TypeAndSpec x)
    | IllegalDestructorCall (TypeAndSpec x) Word Word

    | RunPolarityMismatch (TypeAndSpec x) Word Word

    -- Expected identifier to have polarity
    | UnknownInputService (IdP x)
    | UnknownOutputService (IdP x)

    | NoMainFunction 

$(makeClassyPrisms ''CompileError)

