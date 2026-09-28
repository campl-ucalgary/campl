{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
module MplAsmPasses.FromLambdaLifted.FromLambdaLiftedErrors where

import Optics

import Data.Foldable
import qualified Data.Set as Set

import qualified MplAST.MplCore as MplFrontEnd
import qualified MplAST.MplTypeChecked as MplFrontEnd
import qualified MplUtil.UniqueSupply as MplFrontEnd
import qualified MplPasses.PassesErrorsPprint as MplFrontEnd

import MplAsmAST.MplAsmProg 
import MplAsmAST.MplAsmCore
import MplAsmPasses.PassesErrorsPprint 
import Prettyprinter

data FromLambdaLiftedError 
    -- | Channel, phrase,  phrase definition
    = NoService MplFrontEnd.ChIdentT MplFrontEnd.IdentT --(MplFrontEnd.MplConcObjDefn MplFrontEnd.MplTypeCheckedPhrase)
    | MoreThanOneConsole [MplFrontEnd.ChIdentT]
    | NoPrimitiveEqualityOperator String
    | Error String
  deriving Show

$(makeClassyPrisms ''FromLambdaLiftedError)

pprintFromLambdaLiftedErrors ::
    [FromLambdaLiftedError] ->
    MplAsmDoc
pprintFromLambdaLiftedErrors = vsep . map go
  where
    go = \case
        NoService ch ch_type_ident -> fold
            [ pretty $ "No service type " ++ (ch_type_ident ^. MplFrontEnd.name % MplFrontEnd.nameStr) ++ 
                " for " ++ (show (ch ^. MplFrontEnd.polarity)) ++ " polarity" ++
                " channel " ++ (ch ^. MplFrontEnd.name % MplFrontEnd.nameStr) ++
                " at " ++ (show (ch ^. MplFrontEnd.location % to MplFrontEnd.pprintLoc))
            -- , line
            -- , indent' $ pretty id
            ]

        -- we changed how the primitive operators are being checked, so it happens in type checking now
        NoPrimitiveEqualityOperator error_msg -> fold
            [ pretty ("No primitive operator for given type: " ++ error_msg)
            ]
        
        MoreThanOneConsole (ch:consoles) -> fold
            [ pretty ("Main run process has more than one service channel of type Console: " ++ 
                (ch ^. MplFrontEnd.name % MplFrontEnd.nameStr) ++
                    " at " ++ (show (ch ^. MplFrontEnd.location % to MplFrontEnd.pprintLoc)) ++
                (foldMap (\ch -> ", " ++ (ch ^. MplFrontEnd.name % MplFrontEnd.nameStr) ++
                    " at " ++ (show (ch ^. MplFrontEnd.location % to MplFrontEnd.pprintLoc)))
                    consoles))
            ] 
        MoreThanOneConsole [] -> fold
            [ pretty ("Main run process has more than one service channel of type Console.")
            ] 
        Error err_msg -> fold
            [ pretty err_msg ]

    indent' = indent 4


checkServices :: AsFromLambdaLiftedError e => [MplFrontEnd.ChIdentT] -> [MplFrontEnd.ChIdentT] -> [e]
checkServices input_chs output_chs = checkInputServices ++ checkOutputServices ++ checkNumConsoles
    where
        -- check that input-polarity services are all defined, otherwise collect errors
        checkInputServices :: AsFromLambdaLiftedError e => [e]
        checkInputServices = foldMap isInputService input_chs

        isInputService :: AsFromLambdaLiftedError e => MplFrontEnd.ChIdentT -> [e]
        isInputService ch
            | Just ch_type_ident <- ch ^? MplFrontEnd.chIdentTType % MplFrontEnd._TypeConcWithArgs % _2
                = go ch_type_ident
            -- for some reason the inferred type is set as TypeWithNoArgs instead of TypeConcWithArgs
            | Just ch_type_ident <- ch ^? MplFrontEnd.chIdentTType % MplFrontEnd._TypeWithNoArgs % _2
                = go ch_type_ident
            -- this should not happen though? unless it does some even more whack shit??
            | otherwise = [ _Error # ("No service type for channel " ++ (ch ^. MplFrontEnd.name % MplFrontEnd.nameStr) ++
                    " at " ++ (show (ch ^. MplFrontEnd.location % to MplFrontEnd.pprintLoc))) ]
            where
                go ch_type_ident = let ch_type_name = ch_type_ident ^. MplFrontEnd.name % MplFrontEnd.nameStr in
                    if ch_type_name `Set.member` inputServices then [] else [ _NoService # (ch, ch_type_ident)] 
                inputServices = Set.fromList [ "Console", "Timer" ]

        -- check that output-polarity services are all defined, otherwise collect errors
        checkOutputServices :: AsFromLambdaLiftedError e => [e]
        checkOutputServices = foldMap isOutputService output_chs

        isOutputService :: AsFromLambdaLiftedError e => MplFrontEnd.ChIdentT -> [e]
        isOutputService ch 
            | Just ch_type_ident <- ch ^? MplFrontEnd.chIdentTType % MplFrontEnd._TypeConcWithArgs % _2 
                = go ch_type_ident
            -- for some reason the inferred type is set as TypeWithNoArgs instead of TypeConcWithArgs
            | Just ch_type_ident <- ch ^? MplFrontEnd.chIdentTType % MplFrontEnd._TypeWithNoArgs % _2 
                = go ch_type_ident
            | otherwise = [ _Error # ("No service type for channel " ++ (ch ^. MplFrontEnd.name % MplFrontEnd.nameStr) ++
                    " at " ++ (show (ch ^. MplFrontEnd.location % to MplFrontEnd.pprintLoc))) ]
            where
                go ch_type_ident = let ch_type_name = ch_type_ident ^. MplFrontEnd.name % MplFrontEnd.nameStr in
                    if ch_type_name `Set.member` outputServices then [] else [ _NoService # (ch, ch_type_ident)]  
                outputServices = Set.fromList [ "Terminal" ]

        -- count number of Console channels to check that there is not more than one
        checkNumConsoles :: AsFromLambdaLiftedError e => [e]
        checkNumConsoles = 
            let consoles = [ ch | ch <- input_chs, let ch_type_name = get_ch_type_name ch, ch_type_name == "Console"] in
                if (length consoles) <= 1 then [] else [ _MoreThanOneConsole # (consoles) ]
            where
                get_ch_type_name ch 
                    | Just ch_type_ident <- ch ^? MplFrontEnd.chIdentTType % MplFrontEnd._TypeConcWithArgs % _2 
                        = ch_type_ident ^. MplFrontEnd.name % MplFrontEnd.nameStr
                    -- for some reason the inferred type is set as TypeWithNoArgs instead of TypeConcWithArgs
                    | Just ch_type_ident <- ch ^? MplFrontEnd.chIdentTType % MplFrontEnd._TypeWithNoArgs % _2
                        = ch_type_ident ^. MplFrontEnd.name % MplFrontEnd.nameStr
                    -- since we call checkInputServices first, we should catch any errors related to getting the type above
                    -- so this should never be exec'ed and i guess this is fine
                    | otherwise = ""

