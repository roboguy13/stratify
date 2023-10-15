module Stratify.TypeChecker.Core
  where

import Prim hiding (Type)
import Prelude

import Stratify.Syntax.Core.Term
import Stratify.Syntax.Name

import Bound

import Data.Either
import Data.Unit
import Data.List
import Data.Tuple
import Data.Eq
import Data.Maybe
import Data.Function
import Data.Monoid
import Data.Show
import Data.Bifunctor
import Data.Traversable

type TypeCheck a = Either String a

type Context a b = List (Tuple a b)
type TypeContext a = Context a (Type a)

type OpenedTypeContext a = Context (OpenedVar a) (Type (OpenedVar a))
-- type OpenedTypeContext a = OpenedContext a (Type (OpenedVar a))

-- openContext :: forall a. TypeContext a -> String -> Type (OpenedVar a) -> OpenedTypeContext a
-- openContext ctx var boundVarType =
--   (Tuple (Left (mkCoreName var)) boundVarType)
--     : map (rmap openTerm <<< lmap Right) ctx

-- openContext :: forall a. TypeContext a -> String -> String -> Type a -> OpenedTypeContext a
-- openContext ctx v vTy ty =
--   (Tuple (B (mkCoreName v)) (openTerm ty))
--     :
--   (Tuple (B (mkCoreName vTy)) (openTerm ty))
--     :
--    map (rmap openTerm <<< lmap (F <<< Var)) ctx


-- closeTerm :: forall a. Show a => Term (OpenedVar a) -> TypeCheck (Term a)
-- closeTerm = map join <<< traverse go
--   where
--     go :: OpenedVar a -> TypeCheck (Term a)
--     go (F x) = pure x
--     go (B x) = typeError $ "Variable " <> show x <> " escapes its scope"

-- typeError :: forall a. String -> TypeCheck a
-- typeError = Left

-- -- TODO: Produce a derivation for type checking (even if the term is ill-typed).

-- checkType :: forall a. Show a => Eq a => TypeContext a -> Term a -> Type a -> TypeCheck Unit
-- checkType ctx (Lam v ty bnd) (Forall tyV srcTy tyBnd) = checkTypeScope (openContext ctx v tyV srcTy) bnd tyBnd
-- checkType _ctx IntType (Universe 0) = pure unit
-- checkType _ctx IntType _ = typeError $ "Expected " <> show (Universe 0 :: Term a)
-- checkType _ctx BoolType (Universe 0) = pure unit
-- checkType _ctx IntType _ = typeError $ "Expected " <> show (Universe 0 :: Term a)
-- checkType ctx x ty = do
--   ty' <- inferType ctx x
--   requireSameType ty ty'
-- -- checkType ctx (Var x) ty =
-- --   case lookup x ctx of
-- --       Just xTy -> requireSameType ty xTy
-- -- checkType ctx _ _ = ?b

-- checkTypeScope :: forall a. Show a => Eq a => OpenedTypeContext a -> CoreScope a -> CoreScope a -> TypeCheck Unit
-- checkTypeScope ctx (Scope t) (Scope ty) =
--   ?a
--   -- checkType ctx t ty

-- inferType :: forall a. Show a => Eq a => TypeContext a -> Term a -> TypeCheck (Type a)
-- inferType ctx (Var x) =
--   case lookup x ctx of
--       Just xTy -> pure xTy
--       Nothing -> typeError $ "Cannot find type for variable " <> show x
-- -- inferType ctx (Ann x ty) = checkType ctx x ty
-- inferType _ctx (IntLit _) = pure IntType
-- inferType _ctx (BoolLit _) = pure BoolType
-- inferType _ctx (Lam _v _ty _bnd) = typeError $ "Cannot infer a lambda"
-- inferType ctx (The ty t) = checkType ctx t ty $> ty
-- inferType _ _ = typeError ""

-- requireSameType :: forall a. Show a => Eq a => Type a -> Type a -> TypeCheck Unit
-- requireSameType ty ty' =
--   if ty == ty'
--     then pure unit
--     else typeError $ "Type " <> show ty <> " does not match " <> show ty'

-- extend :: forall a b. Eq a => a -> b -> Context a b -> Context a b
-- extend x y xs = Tuple x y : xs

-- lookup :: forall a b. Eq a => a -> Context a b -> Maybe b
-- lookup z Nil = Nothing
-- lookup z ((Tuple x y) : xs) =
--   if x == z
--     then Just y
--     else lookup z xs

