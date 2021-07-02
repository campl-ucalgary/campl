module MplAsmPasses.FromLambdaLifted.FromLambdaLiftedUtil where

import Optics
import Data.Coerce

-- Front end
import MplAST.MplCore 
import MplAST.MplTypeChecked 
import MplUtil.UniqueSupply

-- Assembler
import qualified MplAsmAST.MplAsmProg as Asm
import qualified MplAsmAST.MplAsmCore as Asm
import qualified MplAsmPasses.PassesErrorsPprint as Asm

import MplAsmPasses.FromLambdaLifted.FromLambdaLiftedAST

-- | overloaded operator for converting an identifier to the assembler identifier
class ToAsmIdP t where
    toAsmIdP :: t -> Asm.IdP MplAsmFromLambdaLifted

instance ToAsmIdP IdentR where
    toAsmIdP identt = coerce $ concat
        [ identt ^. name % coerced 
        , show (coerce (identt ^. identRUniqueTag) :: Word)
        ]

instance ToAsmIdP ChIdentT where
    toAsmIdP chidentt = chidentt ^. chIdentTChIdentR % chIdentRIdentR % to toAsmIdP

