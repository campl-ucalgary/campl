module CMPLCompile where

import CMPLConstructsBag
import CMPLCompileConstructsBag
import AMPLUntaggedConstructBag

import CMPLAST

import Language.PrintAMPL

cmplCompile :: CmplConstructsBag -> String
cmplCompile = 
    printTree
    . untaggedAmplAsmBagToAMPLCODE 
    . cmplCompileConstructsBag

test  = CmplConstructsBag {
    _cmplIncludes = ["potato/test.ampl", "othertest/test.ampl"]
    , _cmplData = [listData]
    , _cmplCodata = [colistdata]

    , _cmplProtocols = [interm]
    , _cmplCoprotocols = [console]

    , _cmplFunction = [adder]
    , _cmplProcess = []

    , _cmplMainRun = mainex
}
  where
    listData = DataCodataClause {
        _dataCodataName = "List"
        , _dataCodataSubclauses = [("Cons", 10), ("Nil", 0)]
    }
    colistdata = DataCodataClause {
        _dataCodataName = "Stream"
        , _dataCodataSubclauses = [("Cons", 10)]
    }

    interm = ProtocolCoprotocolClause {
        _protocolCoprotocolName = "IntTerm"
        , _protocolCoprotocolSubclauses = ["Get", "Put", "Close"]
    }

    console = ProtocolCoprotocolClause {
        _protocolCoprotocolName = "Console"
        , _protocolCoprotocolSubclauses = ["Get", "Put", "Close"]
    }

    adder = FunctionClause {
        _functionName = "adder"
        , _functionArgs = ["a", "b"]
        , _functionExpr = EOp AddInt (EVar "a") (EVar "b")
    }


    mainex = MainClause {
        _mainInputChs = ["inch1", "inch2"]
        , _mainOutputChs = ["outch1", "outch2"]
        , _mainProcessCommands = []
    }
