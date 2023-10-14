module Stratify.Syntax.Name
  where

import Prelude
import Data.Generic.Rep
import Data.Show.Generic

data Name a b =
  Name
  { unique :: b
  , base :: a
  }

instance Eq b => Eq (Name a b) where
  eq (Name x) (Name y) = x.unique == y.unique

instance Ord b => Ord (Name a b) where
  compare (Name x) (Name y) = compare x.unique y.unique

derive instance Functor (Name a)
derive instance Generic (Name a b) _

instance (Show a, Show b) => Show (Name a b) where show = genericShow

