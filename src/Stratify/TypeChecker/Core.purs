module Stratify.TypeChecker.Core
  where

import Prim hiding (Type)
import Prelude

import Stratify.Syntax.Core.Term
import Stratify.Syntax.Name
import Stratify.Ppr
import Stratify.Eval.NbE
import Stratify.Utils

import Bound

import Data.Tuple.Nested
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

typeError :: forall a. String -> TypeCheck a
typeError = Left

type Context = NameEnv Type

checkType :: Context -> Term -> Type -> TypeCheck Unit
checkType ctx x ty = do
  xTy <- inferType ctx x
  requireSameType ty xTy
-- checkType ctx (Var x) ty =
--   case lookup x ctx of
--     Nothing -> typeError $ "Cannot find variable " <> ppr x
--     Just ty' -> requireSameType ty ty'
-- checkType ctx (IntLit _) ty = requireSameType ty IntType
-- checkType ctx (BoolLit _) ty = requireSameType ty BoolType
-- checkType ctx (Op x) ty = checkOp ctx x ty
-- checkType ctx (Not x) ty = do
--   checkType ctx x BoolType
--   requireSameType ty BoolType
-- checkType ctx (App x y) ty = error "App"
--   -- xTy <- inferType ctx x
--   -- Tuple n (Tuple src tgtBody) <- isForall xTy
--   -- checkType ctx y src
--   -- ?a
-- checkType ctx (Lam x argTy body) ty = do
--   Tuple _ (Tuple src tgtBody) <- isForall ty
--   -- let tyVal = evalClosure' argTy
--   let tgt = substHere tgtBody src
--   checkType (extend argTy ctx) body tgtBody
-- checkType ctx (If x y z) ty = do
--   checkType ctx x BoolType
--   checkType ctx y ty
--   checkType ctx z ty
-- checkType ctx (The ty' x) ty = do
--   requireSameType ty ty'
--   checkType ctx x ty'
-- checkType ctx (Forall _ _ _) ty = ?a
-- checkType ctx (Exists _ _ _) ty = ?a
-- checkType ctx IntType ty = requireSameType ty (Universe 0)
-- checkType ctx BoolType ty = requireSameType ty (Universe 0)
-- checkType ctx (Universe k) ty = requireSameType ty (Universe (k + 1))

isForall :: Type -> TypeCheck (Name /\ Type /\ Type)
isForall (Forall x src tgt) = pure $ Tuple x (Tuple src tgt)
isForall ty = typeError $ "Expected forall type, got " <> ppr ty

-- See https://math.andrej.com/2012/11/08/how-to-implement-dependent-type-theory-i/
inferType :: Context -> Term -> TypeCheck Type
inferType ctx (Var x) =
  case lookup x ctx of
    Nothing -> typeError $ "Cannot find variable " <> ppr x
    Just ty' -> pure ty'

inferType ctx (Forall x src tgtBody) = do
  k1 <- inferUniverse ctx src
  k2 <- inferUniverse (extend src ctx) tgtBody
  pure (Universe (max k1 k2))

inferType ctx (Exists x src tgtBody) = do
  k1 <- inferUniverse ctx src
  k2 <- inferUniverse (extend src ctx) tgtBody
  pure (Universe (max k1 k2))

inferType ctx (App x y) = do
  Tuple x (Tuple src tgt) <- inferForall ctx x
  yTy <- inferType ctx y
  requireSameType src yTy
  pure $ substHere y tgt

inferType ctx (Universe k) = pure (Universe (k + 1))

inferType ctx (The ty x) = do
  checkType ctx x ty
  pure ty

inferType ctx (Lam x ty body) = do
  _ <- inferUniverse ctx ty
  tBody <- inferType (extend ty ctx) body
  pure $ Forall x ty tBody

inferType ctx (If x y z) = do
  checkType ctx x BoolType
  yTy <- inferType ctx y
  zTy <- inferType ctx z
  requireSameType yTy zTy
  pure yTy

inferType ctx (Op (Add x y)) = checkBinOp ctx x y IntType IntType $> IntType
inferType ctx (Op (Sub x y)) = checkBinOp ctx x y IntType IntType $> IntType
inferType ctx (Op (Mul x y)) = checkBinOp ctx x y IntType IntType $> IntType
inferType ctx (Op (Div x y)) = checkBinOp ctx x y IntType IntType $> IntType
inferType ctx (Op (Equal x y)) = checkBinOp ctx x y IntType IntType $> BoolType
inferType ctx (Op (Lt x y)) = checkBinOp ctx x y IntType IntType $> BoolType
inferType ctx (Op (And x y)) = checkBinOp ctx x y BoolType BoolType $> BoolType
inferType ctx (Op (Or x y)) = checkBinOp ctx x y BoolType BoolType $> BoolType

inferType ctx (Not x) = do
  checkType ctx x BoolType
  pure BoolType

inferType ctx (IntLit _) = pure IntType
inferType ctx (BoolLit _) = pure BoolType

inferType ctx IntType = pure $ Universe 0
inferType ctx BoolType = pure $ Universe 0
-- inferType ctx x = error "inferType"

checkBinOp :: Context -> Term -> Term -> Type -> Type -> TypeCheck Unit
checkBinOp ctx x y xTy yTy = do
  checkType ctx x xTy
  checkType ctx y yTy

inferForall :: Context -> Term -> TypeCheck (Name /\ Type /\ Type)
inferForall ctx x = do
  ty <- inferType ctx x
  case nf ty of
    Forall x src tgt -> pure (Tuple x (Tuple src tgt))
    _ -> typeError $ "Function expected, got " <> ppr x

inferUniverse :: Context -> Type -> TypeCheck Int
inferUniverse ctx ty = do
  u <- inferType ctx ty
  case nf u of
    Universe k -> pure k
    _ -> typeError $ "Expected type, got " <> ppr ty

checkOp :: Context -> Op -> Type -> TypeCheck Unit
checkOp ctx x ty = pure unit

requireSameType :: Type -> Type -> TypeCheck Unit
requireSameType ty ty' =
  if alphaEquiv ty ty'
    then pure unit
    else typeError $ "Type " <> ppr ty <> " does not match " <> ppr ty'


-- checkType :: Context -> Term -> Type -> TypeCheck Unit
-- checkType ctx (Lam v ty body) (Forall tyV srcTy tyBnd) = do
--   _ <- checkType (extend srcTy ctx) tyBnd (Universe 0)
--   ?a
--   -- val <- evalClosure 
--   -- checkType (extend srcTy ctx) (substHere body)
-- checkType _ctx IntType (Universe 0) = pure unit
-- checkType _ctx IntType _ =
--   typeError $ "Expected " <> show (Universe 0 :: Term)
-- checkType _ _ _ = Left ""


-- type Context a b = List (Tuple a b)
-- type TypeContext a = Context a (Type a)

-- type OpenedTypeContext a = Context (OpenedVar a) (Type (OpenedVar a))
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

-- extend :: forall a b. Eq a => a -> b -> Context a b -> Context a b
-- extend x y xs = Tuple x y : xs

-- lookup :: forall a b. Eq a => a -> Context a b -> Maybe b
-- lookup z Nil = Nothing
-- lookup z ((Tuple x y) : xs) =
--   if x == z
--     then Just y
--     else lookup z xs

