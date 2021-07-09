{-# LANGUAGE TemplateHaskell #-}
module MplCliGenTH where

import MplCliGen 

import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

metaprog :: QuasiQuoter
metaprog = QuasiQuoter 
    { quoteExp = quoteMetaProg 
    , quotePat = error "'metaprog' may only be used for expressions"
    , quoteType = error "'metaprog' may only be used for expressions"
    , quoteDec = error "'metaprog' may only be used for expressions"
    }

quoteMetaProg :: String -> TH.Q TH.Exp
quoteMetaProg inp = do
    loc <- TH.location
    pmprog <- parseMetaProgM 
        ( TH.loc_filename loc
        , fst $ TH.loc_start loc
        , snd $ TH.loc_start loc
        )
        inp
    TH.lift pmprog
