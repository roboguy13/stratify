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
  pure $ substHere (nf tgt) y

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
    Forall n src tgt ->
      pure (Tuple n (Tuple src tgt))
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

