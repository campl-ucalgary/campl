{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module MplAST.MplExt where

data MplPass (c :: Ext)

type MplParsed = MplPass 'Parsed
type MplCmdBinders = MplPass 'CmdBinders
type MplRenamed = MplPass 'Renamed
type MplTypeChecked = MplPass 'TypeChecked

data Ext =
    -- | After bnfc parsing
    Parsed
    -- | This desugars the fork and plug commands 
    -- channel context. 
    | CmdBinders
    -- | Giving each identifier
    -- a unique name
    | Renamed
    -- | Type checking
    | TypeChecked

