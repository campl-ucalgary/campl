{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module CMPLConstructsBag where

import CMPLAST
import MPLIdent

import Text.PrettyPrint.GenericPretty
import Text.PrettyPrint

import Optics.TH


data DataCodataClause ident = DataCodataClause {
    _dataCodataName :: ident
    , _dataCodataSubclauses :: [(ident,Word)]
        -- constructors / destructors, and number of args
}
  deriving (Eq,Show,Read,Generic,Out)


data ProtocolCoprotocolClause ident = ProtocolCoprotocolClause {
    _protocolCoprotocolName :: ident
    , _protocolCoprotocolSubclauses :: [ident]
    -- handles / cohandles (always take 0 args)
}
  deriving (Eq,Show,Read,Generic,Out)


data FunctionClause ident = FunctionClause {
    _functionName :: ident
    , _functionArgs :: [ident]
    , _functionExpr :: Expr ident
}
  deriving (Eq,Show,Read,Generic,Out)


data ProcessClause ident = ProcessClause {
    _processName :: ident
    , _processSeqArgs :: [ident]
    , _processInputChs :: [ident]
    , _processOutputChs :: [ident]
    , _processCommands :: [ProcessCommand ident]
}
  deriving (Eq,Show,Read,Generic,Out)


data MainClause ident = MainClause {
    _mainPosition :: RowColPos
    , _mainInputChs :: [ident]
    , _mainOutputChs :: [ident]
    , _mainProcessCommands :: [ProcessCommand ident]
}
  deriving (Eq,Show,Read,Generic,Out)


data CmplConstructsBag ident = CmplConstructsBag {
    _cmplIncludes :: [String]
    , _cmplData :: [DataCodataClause ident]
    , _cmplCodata :: [DataCodataClause ident]

    , _cmplProtocols :: [ProtocolCoprotocolClause ident]
    , _cmplCoprotocols :: [ProtocolCoprotocolClause ident]

    , _cmplFunction :: [FunctionClause ident] 
    , _cmplProcess :: [ProcessClause ident]

    , _cmplMainRun :: MainClause ident
    
}
  deriving (Eq,Show,Read,Generic,Out)

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
