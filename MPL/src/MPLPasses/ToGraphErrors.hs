{-# LANGUAGE TemplateHaskell #-}
module MPLPasses.ToGraphErrors where

import Optics
import MPLAST.MPLASTCore


data ToGraphErrors 

$(makeClassyPrisms ''ToGraphErrors)
    




