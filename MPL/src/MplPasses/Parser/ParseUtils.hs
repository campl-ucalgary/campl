{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MplPasses.Parser.ParseUtils where
import Optics
import Data.Maybe

import qualified MplPasses.Parser.BnfcParse as B

import MplAST.MplCore
import MplAST.MplParsed

import MplPasses.Parser.ParseErrors 

import Control.Monad.Writer
import Control.Monad.Except
import Text.Read

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))

import Control.Arrow
import Data.Kind

type BnfcParse bnfc ast = 
    forall e m.
    ( AsParseErrors e
    , MonadWriter [e] m
    , MonadError () m 
    ) =>
    bnfc -> m ast

newtype BnfcParseM a = BnfcParseM {
        unBnfcParseM :: ExceptT () (Writer [ParseErrors]) a
    }
  deriving 
    ( Functor
    , Applicative
    , Monad
    , MonadError ()
    , MonadWriter [ParseErrors] )

class ToLocation location where
    toLocation :: location -> Location

instance ToLocation (Int,Int) where
    toLocation = Location

instance ToLocation B.PIdent  where
    toLocation (B.PIdent (pos, str)) = toLocation pos

instance ToLocation B.UIdent  where
    toLocation (B.UIdent (pos, str)) = toLocation pos

instance ToLocation B.Par  where
    toLocation (B.Par (pos, str)) = toLocation pos

instance ToLocation B.Tensor  where
    toLocation (B.Tensor (pos, str)) = toLocation pos

instance ToLocation B.LBracket  where
    toLocation (B.LBracket (pos, str)) = toLocation pos

instance ToLocation B.RBracket  where
    toLocation (B.RBracket (pos, str)) = toLocation pos

instance ToLocation B.LSquareBracket  where
    toLocation (B.LSquareBracket (pos, str)) = toLocation pos

instance ToLocation B.RSquareBracket  where
    toLocation (B.RSquareBracket (pos, str)) = toLocation pos


instance ToLocation B.Colon  where
    toLocation (B.Colon (pos, str)) = toLocation pos

instance ToLocation B.NullPattern  where
    toLocation (B.NullPattern (pos,str)) = toLocation pos

instance ToLocation B.PInteger  where
    toLocation (B.PInteger (pos,str)) = toLocation pos

instance ToLocation IdentP  where
    toLocation =  view identPLocation


class ToIdentP ident where
    toIdentP :: Namespace -> ident -> IdentP

instance ToIdentP B.PIdent where
    toIdentP n (B.PIdent (pos, str)) = 
        IdentP (Name str) (Location pos) n

instance ToIdentP B.UIdent where
    toIdentP n (B.UIdent (pos, str)) = 
        IdentP (Name str) (Location pos) n

instance ToIdentP B.PInteger where
    toIdentP n (B.PInteger (pos, str)) = 
        IdentP (Name str) (Location pos) n

instance ToIdentP B.TypeHandleName where
    toIdentP n (B.TYPE_HANDLE_NAME uident) = 
        toIdentP n uident

instance ToIdentP B.ForkChannel where
    toIdentP n (B.FORK_CHANNEL uident) = 
        toIdentP n uident

instance ToIdentP ((Int,Int), String) where
    toIdentP n (pos, str) =
        IdentP (Name str) (Location pos) n

instance ToIdentP B.Case where
    toIdentP n (B.Case uident) = 
        toIdentP n uident

instance ToIdentP B.ChId where
    toIdentP n (B.ChId uident) = 
        toIdentP n uident

instance ToIdentP B.Fork where
    toIdentP n (B.Fork uident) = 
        toIdentP n uident

instance ToIdentP B.Split where
    toIdentP n (B.Split uident) = 
        toIdentP n uident

instance ToIdentP B.HPut where
    toIdentP n (B.HPut uident) = 
        toIdentP n uident

instance ToIdentP B.HCase where
    toIdentP n (B.HCase uident) = 
        toIdentP n uident

instance ToIdentP B.Put where
    toIdentP n (B.Put uident) = 
        toIdentP n uident

instance ToIdentP B.Get where
    toIdentP n (B.Get uident) = 
        toIdentP n uident

instance ToIdentP B.Halt where
    toIdentP n (B.Halt uident) = 
        toIdentP n uident

instance ToIdentP B.Close where
    toIdentP n (B.Close uident) = 
        toIdentP n uident


toChIdentP :: ToIdentP ident => ident -> IdentP
toChIdentP = toIdentP ChannelLevel

toTermIdentP :: ToIdentP ident => ident -> IdentP
toTermIdentP = toIdentP TermLevel

toTypeIdentP :: ToIdentP ident => ident -> IdentP
toTypeIdentP = toIdentP TypeLevel


toSpanLocation :: 
    ( ToLocation l1
    , ToLocation l2 ) =>
    l1 -> 
    l2 -> 
    Location
toSpanLocation a b = 
    Span (minLoc a') (maxLoc b')
  where
    a' = toLocation a 
    b' = toLocation b

    minLoc (Location a) = a
    minLoc (Span a b) = a

    maxLoc (Location a) = a
    maxLoc (Span a b) = b

traverseTryEach ::
    ( AsParseErrors e
    , MonadError () m 
    , MonadWriter [e] m 
    , Traversable t) => 
     (a -> m b) -> t a -> m (t b)
traverseTryEach f ns = do
    ns' <- traverse f' ns
    if allOf traversed (has _Just) ns'
        then return $ ns' & traversed %~ fromJust
        else throwError ()
  where
    f' n = fmap Just (f n) `catchError` const (return Nothing)


pIntegerToLocationInt a@(B.PInteger (_, str)) =
    (toLocation a,) <$> (readMaybe str :: Maybe Int)

