{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module CMPLConstructsBag where

import CMPLAST

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

import Optics.TH

data DataCodataClause = DataCodataClause {
    _dataCodataName :: String
    , _dataCodataSubclauses :: [(String,Word)]
        -- constructors / destructors, and number of args
}
  deriving (Eq,Show,Read,Generic,Out)


data ProtocolCoprotocolClause = ProtocolCoprotocolClause {
    _protocolCoprotocolName :: String
    , _protocolCoprotocolSubclauses :: [String]
    -- handles / cohandles (always take 0 args)
}
  deriving (Eq,Show,Read,Generic,Out) 

data FunctionClause = FunctionClause {
    _functionName :: String
    , _functionArgs :: [String]
    , _functionExpr :: Expr 
}
  deriving (Eq,Show,Read,Generic,Out)


data ProcessClause = ProcessClause {
    _processName :: String
    , _processSeqArgs :: [String]
    , _processInputChs :: [String]
    , _processOutputChs :: [String]
    , _processCommands :: [ProcessCommand]
}
  deriving (Eq,Show,Read,Generic,Out)


data MainClause = MainClause {
    _mainInputChs :: [String]
    , _mainOutputChs :: [String]
    , _mainProcessCommands :: [ProcessCommand]
}
  deriving (Eq,Show,Read,Generic,Out)


data CmplConstructsBag = CmplConstructsBag {
    _cmplIncludes :: [String]
    , _cmplData :: [DataCodataClause]
    , _cmplCodata :: [DataCodataClause]

    , _cmplProtocols :: [ProtocolCoprotocolClause]
    , _cmplCoprotocols :: [ProtocolCoprotocolClause]

    , _cmplFunction :: [FunctionClause] 
    , _cmplProcess :: [ProcessClause]

    , _cmplMainRun :: MainClause
} deriving (Eq,Show,Read,Generic,Out) 
$(
    concat <$> traverse makeLenses 
        [ ''CmplConstructsBag
        , ''DataCodataClause
        , ''ProtocolCoprotocolClause
        , ''FunctionClause
        , ''ProcessClause
        , ''MainClause
        ]
 )
