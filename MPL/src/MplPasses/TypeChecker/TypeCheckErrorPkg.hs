{-# LANGUAGE TemplateHaskell #-}
module MplPasses.TypeChecker.TypeCheckErrorPkg where

import Optics 
import Optics.State.Operators 

data TypeCheckErrorPkg external internal = TypeCheckErrorPkg {
    _pkgExternalError :: [external]
    , _pkgInternalError :: [internal]
}

$(makeLenses ''TypeCheckErrorPkg)

instance Semigroup (TypeCheckErrorPkg external internal ) where
    TypeCheckErrorPkg a0 b0@(_:_) <> TypeCheckErrorPkg a1 b1 = TypeCheckErrorPkg mempty (b0 <> b1)
    TypeCheckErrorPkg a0 b0 <> TypeCheckErrorPkg a1 b1@(_:_) = TypeCheckErrorPkg mempty (b0 <> b1)
    ~(TypeCheckErrorPkg a0 b0) <> ~(TypeCheckErrorPkg a1 b1) = TypeCheckErrorPkg (a0 <> a1) (b0 <> b1)

instance Monoid (TypeCheckErrorPkg internal external) where
    mempty = TypeCheckErrorPkg mempty mempty

instance AsEmpty (TypeCheckErrorPkg internal external) where
    _Empty = nearly mempty f
      where
        f pkg = pkg ^. pkgInternalError % to (has _Empty) 
            && pkg ^. pkgExternalError % to (has _Empty)

_InternalError :: Prism' (TypeCheckErrorPkg external internal) [internal] 
_InternalError = prism' cts prj
  where
    cts n = TypeCheckErrorPkg mempty n
    prj (TypeCheckErrorPkg [] n ) = Just n
    prj _ = Nothing

_ExternalError :: Prism' (TypeCheckErrorPkg external internal ) [external] 
_ExternalError = prism' cts prj
  where
    cts n = TypeCheckErrorPkg n mempty
    prj (TypeCheckErrorPkg n []) = Just n
    prj _ = Nothing


collectPkgErrors :: TypeCheckErrorPkg a a -> [a]
collectPkgErrors pkg  
    | hasn't (pkgInternalError % _Empty) pkg = 
        pkg ^. pkgInternalError
    | otherwise = 
        pkg ^. pkgExternalError
