{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module MplAST.MplExt where

data MplPass (c :: Ext)

type MplParsed = MplPass 'Parsed
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

