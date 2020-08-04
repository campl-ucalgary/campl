{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module MPLAST.MPLExt where


data MplPass (c :: Ext)

data Ext =
    Parsed
    | AlphaRenamedTypeChecked
