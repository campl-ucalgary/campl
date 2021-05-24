{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE CPP #-}
module MplAST.MplPattern where

import MplAST.MplIdent

import Optics
import Data.Functor.Foldable.TH
import Data.Functor.Foldable hiding (fold)

import Data.Function
import Data.List.NonEmpty (NonEmpty (..))
import Data.List
import Data.Kind
import Data.Maybe
import Data.Foldable 
import qualified Data.List.NonEmpty as NE 

import GHC.Generics
import Data.Data
import Data.Typeable

{- Module for the AST data type of a pattern in MPL...
 -}

type family XMplPattern x

type family XPConstructor x
type family XPSimpleConstructor x
type family XPSimpleConstructorArgs x

type family XPRecord x
type family XPRecordPhrase x
type family XPVar x
type family XPNull x

type family XPUnit x
type family XPTuple x
type family XPString x
type family XPInt x
type family XPBool x
type family XPChar x
type family XPList x
type family XPListCons x

type family XXPattern x


type ForallMplPattern (c :: Type -> Constraint) x = 
     ( c (XPConstructor x)
     , c (XPSimpleConstructor x)
     , c (XPSimpleConstructorArgs x)
     , c (XPRecord x)
     , c (XPRecordPhrase x)
     , c (XPVar x)
     , c (XPNull x)
     , c (XPUnit x)
     , c (XPTuple x)
     , c (XPString x)
     , c (XPInt x)
     , c (XPBool x)
     , c (XPChar x)
     , c (XPList x)
     , c (XPListCons x)
     , c (XXPattern x)
     )

data MplPattern x =
    PConstructor !(XPConstructor x) (IdP x) [MplPattern x]
    | PSimpleConstructor !(XPSimpleConstructor x) (IdP x) (XPSimpleConstructorArgs x)
    | PRecord !(XPRecord x) (NonEmpty (XPRecordPhrase x, IdP x, MplPattern x) )
    | PVar !(XPVar x) (IdP x)
    | PNull !(XPNull x) 


    -- { _pRecordPhrase :: NonEmpty (ident , (calldef, Pattern typedef calldef ident)) , _pType :: typedef }
    -- | PList { _pList :: [Pattern typedef calldef ident], _pType :: typedef }
    -- | PTuple { _pTuple :: (Pattern typedef calldef ident, NonEmpty (Pattern typedef calldef ident)), _pType :: typedef }
    -- | PString { _pString :: String, _pType :: typedef }
    -- | PInt { _pInt :: (ident, Int), _pType :: typedef }
 
    -- built in patterns
    | PUnit !(XPUnit x)
    | PTuple !(XPTuple x) (MplPattern x, MplPattern x, [MplPattern x])
    | PList !(XPList x) [MplPattern x]
    | PString !(XPString x) String
    | PListCons !(XPListCons x) (MplPattern x) (MplPattern x)

    | PChar !(XPChar x) Char
    | PInt !(XPInt x) Int
    | PBool !(XPBool x) Bool

    | XPattern !(XXPattern x)

deriving instance (ForallMplPattern Show x, Show (IdP x)) => Show (MplPattern x)

pattern UPConstructor ident args <- PConstructor () ident args
  where
    UPConstructor ident args = PConstructor () ident args

pattern UPRecord phrases <- PRecord () (fmap (\((),b,c) -> (b,c)) -> phrases)
  where
    UPRecord phrases = PRecord () $ fmap (\(b,c) -> ((),b,c)) phrases

pattern UPVar id <- PVar () id
  where
    UPVar id = PVar () id


{-
data Pattern typedef calldef ident =
    PConstructor { _pConstructor :: ident
        , _pConstructorCallDef :: calldef
        , _pConstructorArgs :: [Pattern typedef calldef ident]
        , _pType :: typedef}
    | PUnit { _pUnit :: ident, _pType :: typedef }
    | PRecord { 
        _pRecordPhrase :: NonEmpty (ident , (calldef, Pattern typedef calldef ident))
                , _pType :: typedef }
    | PList { _pList :: [Pattern typedef calldef ident], _pType :: typedef }
    | PTuple { _pTuple :: (Pattern typedef calldef ident, NonEmpty (Pattern typedef calldef ident)), _pType :: typedef }
    | PVar { _pVar :: ident, _pType :: typedef }
    | PString { _pString :: String, _pType :: typedef }
    | PInt { _pInt :: (ident, Int), _pType :: typedef }
    | PNull  { _pNull :: ident, _pType :: typedef }
  deriving ( Read, Show, Generic, Data, Eq )
  -}


$(makeLenses ''MplPattern)
$(concat <$> traverse makeClassyPrisms 
    [ ''MplPattern ] 
 )


$(makeBaseFunctor ''MplPattern)

collectPVarIdPs ::
    MplPattern x ->
    [IdP x]
collectPVarIdPs = cata f
  where
    f (PVarF _ ident) = [ident]
    f n = fold n
