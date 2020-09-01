{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module MplPasses.TypeChecker.TypeCheckCallErrors where

import Optics

import MplAST.MplCore
import MplAST.MplParsed
import MplAST.MplRenamed
import MplAST.MplTypeChecked

data TypeCheckCallErrors = 
    -- Calling something that failed to compile..
    ---------------------
    CannotCallTerm (IdP MplRenamed)
    | CannotCallTypeCts (IdP MplRenamed)

    -- Function definition errors...
    --------------------------------
    | IllegalPattDataCallGotCodataInstead 
        (MplPattern MplRenamed) (MplTypePhrase MplTypeChecked (SeqObjTag CodataDefnTag)) 
            -- pattern, illegal codata call
    | IllegalExprDataCallGotCodataInstead 
        (MplExpr MplRenamed) (MplTypePhrase MplTypeChecked (SeqObjTag CodataDefnTag)) 
            -- expr, illegal codata call
    | IllegalExprCodataCallGotDataInstead 
        -- (MplPattern MplRenamed) (MplTypePhrase MplRenamed (SeqObjTag CodataDefnTag)) 
        (MplExpr MplRenamed) (MplTypePhrase MplTypeChecked (SeqObjTag DataDefnTag)) 
            -- expr, codata call


    | ExpectedPattCodataCallButGotADataCall 
        (MplTypePhrase MplRenamed (SeqObjTag CodataDefnTag)) (MplPattern MplRenamed)

  deriving Show

$(makeClassyPrisms ''TypeCheckCallErrors)
