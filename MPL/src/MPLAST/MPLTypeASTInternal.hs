module MPLAST.MPLTypeASTInternal where

import Optics
import MPLAST.MPLTypeAST 
import MPLAST.MPLProgI
import MPLAST.MPLASTIdent

_BnfcTypeTopBotF :: Eq t => Prism' (ConcTypeF BnfcIdent t) ()
_BnfcTypeTopBotF = only (_TypeTopBotF # mkBnfcIdent (_InternalConcTypeParser # InternalTopBot) )

_BnfcTypeGetF :: Prism' (ConcTypeF BnfcIdent t) (t, t)
_BnfcTypeGetF = prism' cts prj
  where
    cts (seq, conc) = _TypeGetF # (mkBnfcIdent (_InternalConcTypeParser # InternalGet), seq, conc)

    prj (TypeGetF (BnfcIdent (str,(-1,-1))) seq conc) = 
        fmap (const (seq,conc)) $ str ^? _InternalConcTypeParser % _InternalGet
    prj _ = Nothing

_BnfcTypePutF :: Prism' (ConcTypeF BnfcIdent t) (t, t)
_BnfcTypePutF = prism' cts prj
  where
    cts (seq, conc) = _TypePutF # (mkBnfcIdent $ _InternalConcTypeParser # InternalPut, seq, conc)

    prj (TypePutF (BnfcIdent (str,(-1,-1))) seq conc) = 
        fmap (const (seq,conc)) $ str ^? _InternalConcTypeParser % _InternalPut
    prj _ = Nothing


mkBnfcIdent :: 
    String ->
    BnfcIdent
mkBnfcIdent str = (BnfcIdent (str, (-1,-1)))
