module MplUtil.Data.Tuple.Optics where

import Optics

{- Module for extra Optic helpers that are not given
 - in the Optics library...
 -}

associated :: Iso (a, (b, c)) (d, (e, f))
            ((a, b), c) ((d, e), f)
associated = iso to from
  where
    to (a,(b,c)) = ((a,b),c)
    from ((d,e),f) = (d,(e,f))




