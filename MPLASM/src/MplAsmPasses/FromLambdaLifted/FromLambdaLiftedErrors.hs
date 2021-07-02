{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
module MplAsmPasses.FromLambdaLifted.FromLambdaLiftedErrors where

import Optics

import Data.Foldable

import qualified MplAST.MplCore as MplFrontEnd
import qualified MplAST.MplTypeChecked as MplFrontEnd
import qualified MplUtil.UniqueSupply as MplFrontEnd

import MplAsmAST.MplAsmProg 
import MplAsmAST.MplAsmCore
import MplAsmPasses.PassesErrorsPprint 
import Data.Text.Prettyprint.Doc

data FromLambdaLiftedError 
    -- | Channel, phrase,  phrase definition
    = NoService MplFrontEnd.ChIdentT MplFrontEnd.IdentT (MplFrontEnd.MplConcObjDefn MplFrontEnd.MplTypeCheckedPhrase)
    | NoPrimitiveEqualityOperator 
  deriving Show

$(makeClassyPrisms ''FromLambdaLiftedError)

pprintFromLambdaLiftedErrors ::
    [FromLambdaLiftedError] ->
    MplAsmDoc
pprintFromLambdaLiftedErrors = vsep . map go
  where
    go = \case
        NoService ch phr phrdefn -> fold
            [ pretty "No service for channel: TODO -- actually give the channel name."
            , line
            -- , indent' $ pretty id
            ]

        NoPrimitiveEqualityOperator  ->  fold
            [ pretty "No equality primitive operator for given type."
            ]

    indent' = indent 4
