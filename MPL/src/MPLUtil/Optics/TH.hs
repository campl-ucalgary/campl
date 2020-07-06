module MPLUtil.Optics.TH where

import Optics

mplMakeFieldLabels = makeFieldLabelsWith (fieldLabelsRules & lensField .~ underscoreNoPrefixNamer)

