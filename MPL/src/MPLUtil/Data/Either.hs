module MPLUtil.Data.Either where

import Optics.Fold
import Optics.Getter
import Data.Either

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

collectOnlyIfNoLeftsGetter :: Getter [Either e a] (Either (NonEmpty e) [a])
collectOnlyIfNoLeftsGetter = to get
  where
    get lst = case partitionEithers lst of
                ([], res) -> Right res
                (errs, _) -> Left (NE.fromList errs)

collectsOnlyIfNoLeftsGetter :: Getter [Either (NonEmpty e) a] (Either (NonEmpty e) [a])
collectsOnlyIfNoLeftsGetter = to get
  where
    get lst = case partitionEithers lst of
                ([], res) -> Right res
                ((e :| err) : errs, _) -> Left $
                    e :| err ++ (foldr (\n acc -> NE.toList n ++ acc) [] errs)
