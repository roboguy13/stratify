module Stratify.Syntax.Fresh
  where

import Prelude

import Stratify.Syntax.Name

import Control.Monad
import Control.Monad.State
import Data.Identity
import Control.Comonad (extract)
import Data.Traversable

import Bound

newtype FreshT m a = FreshT (StateT Int m a)

type Fresh = FreshT Identity

derive newtype instance functorFreshT :: Functor m => Functor (FreshT m)
derive newtype instance applyFreshT :: Monad m => Apply (FreshT m)
derive newtype instance applicativeFreshT :: Monad m => Applicative (FreshT m)
derive newtype instance bindFreshT :: Monad m => Bind (FreshT m)
derive newtype instance monadFreshT :: Monad m => Monad (FreshT m)

fresh :: forall a m. Monad m => a -> FreshT m (Name a Int)
fresh base = FreshT do
  n <- get
  _ <- modify (_ + 1)
  pure $ Name { unique: n, base: base }

runFreshT :: forall a m. Monad m => FreshT m a -> m a
runFreshT (FreshT m) = evalStateT m 0

runFresh :: forall a. Fresh a -> a
runFresh = extract <<< runFreshT

-- open :: forall b f a. Scope b f a -> Fresh (f a)
-- open = ?a

-- close :: forall b f a. f a -> b -> Scope b f a
-- close = ?a

-- open s = sequence (instantiate fresh s)

