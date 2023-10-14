module Stratify.Unify
  where

import Bound

import Data.Maybe
import Data.List
import Data.Either
import Data.Tuple
import Data.Eq
import Data.Boolean
import Data.Function
import Control.Monad
import Data.HeytingAlgebra

-- class Unify f where
--     -- isVar . mkVar = id
--   isVar :: forall a. f a -> Maybe a
--   mkVar :: forall a. a -> f a

--   match :: forall a. f a -> f a -> Maybe (List (Tuple (f a) (f a)))
--   children :: forall a. f a -> List (f a)

--     -- freeVars (mkVar x) = [x]
--   freeVars :: forall a. f a -> List a

-- data Equal :: (Type -> Type) -> Type -> Type
-- data Equal f a = Equal a (f a)

-- type Subst f a = List (Equal f a)

-- type UnifyResult :: (Type -> Type) -> Type -> Type
-- type UnifyResult f a = Either String (Subst f a)

-- data OccursCheck = NoOccursCheck | WithOccursCheck

-- -- applySubst :: Monad f => f a -> Subst f a -> f a
-- -- applySubst = ?a

-- unify :: forall f a. (Unify f) => Eq a => OccursCheck -> f a -> f a -> UnifyResult f a
-- unify occCheck = unifyUsingSubst occCheck Nil

-- unifyUsingSubst :: forall f a. (Unify f) => Eq a => OccursCheck -> Subst f a -> f a -> f a -> UnifyResult f a
-- unifyUsingSubst occCheck subst x y
--   | Just vX <- isVar x = unifyVar occCheck subst vX y
--   | Just vY <- isVar y = unifyVar occCheck subst vY x
--   | Just ps <- match x y = unifyLists occCheck subst ps
--   | otherwise = Left "Cannot unify"

-- unifyLists :: forall f a. (Unify f) => Eq a => OccursCheck -> Subst f a -> List (Tuple (f a) (f a)) -> UnifyResult f a
-- unifyLists _occCheck subst Nil = pure subst
-- unifyLists occCheck subst (Tuple x y : rest) = do
--   subst' <- unifyUsingSubst occCheck subst x y
--   unifyLists occCheck subst' rest

-- unifyVar :: forall f a. (Unify f) => Eq a => OccursCheck -> Subst f a -> a -> f a -> UnifyResult f a
-- unifyVar occCheck subst v x
--   | Just y <- lookupSubst subst v = unifyUsingSubst occCheck subst y x 

--   | Just vX <- isVar x, Just x' <- lookupSubst subst vX =
--       unifyUsingSubst occCheck subst (mkVar v) x'

--   | WithOccursCheck <- occCheck, not (occursCheck subst v x) = Left "Failed occurs check"

--   | otherwise = pure $ extendSubst subst v x

-- occursCheck :: forall f a. Unify f => Eq a => Subst f a -> a -> f a -> Boolean
-- occursCheck subst v x
--   | Just vX <- isVar x, v == vX = true
--   | Just vX <- isVar x, Just x' <- lookupSubst subst vX = occursCheck subst v x'
--   | otherwise = any (occursCheck subst v) (children x)


-- lookupSubst :: forall f a. Eq a => Subst f a -> a -> Maybe (f a)
-- lookupSubst Nil _ = Nothing
-- lookupSubst ((Equal v x) : xs) z =
--   if v == z
--     then Just x
--     else lookupSubst xs z

-- extendSubst :: forall f a. Subst f a -> a -> f a -> Subst f a
-- extendSubst xs v y = Equal v y : xs

