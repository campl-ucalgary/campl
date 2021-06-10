module MplAsmPasses.PassesErrorsPprint where

import Data.Text.Prettyprint.Doc

-- | wrapper for the 'Doc' data type
type MplAsmDoc = Doc MplAsmAnn

-- | Provides no useful information yet
data MplAsmAnn = MplAsmAnn 
