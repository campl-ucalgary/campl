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

{- Module for defining utility functions when parsing
 -
 -}

type BnfcParse bnfc ast = 
    forall e m.
    ( AsParseErrors e
    , MonadWriter [e] m
    , MonadError () m 
    ) =>
    bnfc -> m ast

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
    toLocation =  view (identPNameOcc % location)

class ToNameOcc ident where
    toNameOcc :: ident -> NameOcc

instance ToNameOcc ((Int,Int), String) where
    toNameOcc (pos, name) = NameOcc (Name name) (toLocation pos)

instance ToNameOcc B.Case where
    toNameOcc (B.Case uident) = 
        toNameOcc uident

instance ToNameOcc B.ChId where
    toNameOcc (B.ChId uident) = 
        toNameOcc uident

instance ToNameOcc B.Fork where
    toNameOcc (B.Fork uident) = 
        toNameOcc uident

instance ToNameOcc B.Split where
    toNameOcc (B.Split uident) = 
        toNameOcc uident

instance ToNameOcc B.HPut where
    toNameOcc (B.HPut uident) = 
        toNameOcc uident

instance ToNameOcc B.HCase where
    toNameOcc (B.HCase uident) = 
        toNameOcc uident

instance ToNameOcc B.Put where
    toNameOcc (B.Put uident) = 
        toNameOcc uident

instance ToNameOcc B.Get where
    toNameOcc (B.Get uident) = 
        toNameOcc uident

instance ToNameOcc B.Halt where
    toNameOcc (B.Halt uident) = 
        toNameOcc uident

instance ToNameOcc B.Close where
    toNameOcc (B.Close uident) = 
        toNameOcc uident

instance ToNameOcc B.PIdent where
    toNameOcc (B.PIdent posstr) = 
        toNameOcc posstr

instance ToNameOcc B.UIdent where
    toNameOcc (B.UIdent posstr) = 
        toNameOcc posstr

instance ToNameOcc B.PInteger where
    toNameOcc (B.PInteger posstr) = 
        toNameOcc posstr

instance ToNameOcc B.TypeHandleName where
    toNameOcc (B.TYPE_HANDLE_NAME uident) = 
        toNameOcc uident

instance ToNameOcc B.ForkChannel where
    toNameOcc (B.FORK_CHANNEL uident) = 
        toNameOcc uident

instance ToNameOcc B.LBracket where
    toNameOcc (B.LBracket uident) = 
        toNameOcc uident

instance ToNameOcc B.Tensor where
    toNameOcc (B.Tensor uident) = 
        toNameOcc uident

instance ToNameOcc B.Par where
    toNameOcc (B.Par uident) = 
        toNameOcc uident

instance ToNameOcc IdentP where
    toNameOcc n = n ^. identPNameOcc

toChIdentP :: ToNameOcc ident => ident -> IdentP
toChIdentP = flip IdentP ChannelLevel . toNameOcc

toTermIdentP :: ToNameOcc ident => ident -> IdentP
toTermIdentP = flip IdentP TermLevel . toNameOcc

toTypeIdentP :: ToNameOcc ident => ident -> IdentP
toTypeIdentP = flip IdentP TypeLevel . toNameOcc


toSpanLocation :: 
    ( ToLocation l1
    , ToLocation l2 ) =>
    l1 -> 
    l2 -> 
    Location
toSpanLocation a b =  a'
    -- Span (minLoc a') (maxLoc b')
  where
    a' = toLocation a 
    b' = toLocation b

    minLoc (Location a) = a
    -- minLoc (Span a b) = a

    maxLoc (Location a) = a
    -- maxLoc (Span a b) = b

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

