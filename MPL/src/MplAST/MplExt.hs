{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module MplAST.MplExt where

data MplPass (c :: Ext)

type MplParsed = MplPass 'Parsed
type MplCmdContext = MplPass 'CmdContext
type MplRenamed = MplPass 'Renamed
type MplTypeChecked = MplPass 'TypeChecked

data Ext =
    -- | After bnfc parsing
    Parsed
    -- | Giving each identifier
    -- a unique name
    | Renamed
    -- | Type checking
    | TypeChecked

-- | Computing the variable closure of of the fork and plug commands 
-- (this does NOT affect anything other than commands). 
-- Note that this is done immediately before renaming
-- and there is not a dedicated pass for this action.
--
-- The algorithm is as follows. First, pass the bound variables 
-- down to each of the commands in a top down manner.
-- Then, with a fold, go bottom up to compute the free variables
-- and bind to fork and plug as needed in order
data MplCmdCxtPasses (c :: CmdCxtExt)

data CmdCxtExt =
    Bounded
    | CmdCxt

