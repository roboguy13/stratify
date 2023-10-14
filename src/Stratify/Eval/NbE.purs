-- Normalization-by-Evaluation
module Stratify.Eval.NbE
  where

import Prim hiding (Type)
import Prelude

import Stratify.Syntax.Core.Term

import Bound

import Data.List
import Data.Tuple

import Data.Either
import Data.Maybe

import Data.Generic.Rep

import Data.Traversable
import Data.Show.Generic

import Data.Functor.Invariant
import Data.Bifunctor

import Unsafe.Coerce
import Effect.Unsafe
import Effect.Exception

data Value a
  = VIntLit Int
  | VBoolLit Boolean
  | VLam (TypeValue a) (Closure a)
  | VForall (TypeValue a) (Closure a)
  | VExists (TypeValue a) (Closure a)
  | VIntType
  | VBoolType
  | VUniverse Int
  | VNeutral (TypeValue a) (Neutral a)

data Neutral a
  = NVar a
  | NAdd (Neutral a) (Normal a)
  | NSub (Neutral a) (Normal a)
  | NMul (Neutral a) (Normal a)
  | NDiv (Neutral a) (Normal a)
  | NEqual (Neutral a) (Normal a)
  | NLt (Neutral a) (Normal a)
  | NAnd (Neutral a) (Normal a)
  | NOr (Neutral a) (Normal a)

  | NNot (Neutral a)
  | NApp (Neutral a) (Normal a)
  | NIf (Neutral a) (Normal a) (Normal a)

type Normal = Value

-- data Normal a =
--   Normal
--   { ty :: TypeValue a
--   , value :: Value a
--   }

derive instance Generic (Value a) _
derive instance Generic (Neutral a) _
derive instance Generic (Closure a) _
-- derive instance Generic (Normal a) _

instance Show a => Show (Value a) where show x = genericShow x
instance Show a => Show (Neutral a) where show x = genericShow x
instance Show a => Show (Closure a) where show _ = "<closure>"
-- instance Show a => Show (Normal a) where show = genericShow

type TypeValue = Value

type Env a = List (Tuple a (Value a))

-- data Closure a =
--   Closure
--   { env :: Env a
--   , name :: String
--   , body :: Term a
--   }

data Closure a =
  Closure
  { argName :: String
  , argType :: TypeValue a
  , fn :: Value a -> Eval (Value a)
  }

instance Invariant Value where
  imap _ _ (VIntLit i) = VIntLit i
  imap _ _ (VBoolLit b) = VBoolLit b
  imap f g (VLam ty c) = VLam (imap f g ty) (imap f g c)
  imap f g (VForall ty c) = VForall (imap f g ty) (imap f g c)
  imap f g (VExists ty c) = VExists (imap f g ty) (imap f g c)
  imap _ _ VIntType = VIntType
  imap _ _ VBoolType = VBoolType
  imap _ _ (VUniverse k) = VUniverse k
  imap f g (VNeutral ty n) = VNeutral (imap f g ty) (imap f g n)

instance Invariant Neutral where
  imap f _ (NVar x) = NVar (f x)
  imap f g (NAdd x y) = NAdd (imap f g x) (imap f g y)
  imap f g (NSub x y) = NSub (imap f g x) (imap f g y)
  imap f g (NMul x y) = NMul (imap f g x) (imap f g y)
  imap f g (NEqual x y) = NEqual (imap f g x) (imap f g y)
  imap f g (NDiv x y) = NDiv (imap f g x) (imap f g y)
  imap f g (NLt x y) = NLt (imap f g x) (imap f g y)
  imap f g (NAnd x y) = NAnd (imap f g x) (imap f g y)
  imap f g (NOr x y) = NOr (imap f g x) (imap f g y)
  imap f g (NNot x) = NNot (imap f g x)
  imap f g (NApp x y) = NApp (imap f g x) (imap f g y)
  imap f g (NIf x y z) = NIf (imap f g x) (imap f g y) (imap f g z)


-- instance Invariant Normal where
--   imap f g (Normal n) =
--     Normal
--     { ty: imap f g n.ty
--     , value: imap f g n.value
--     }

instance Invariant Closure where
  imap f g (Closure c) =
    Closure
    { argName: c.argName
    , argType: imap f g c.argType
    , fn: \x -> map (imap f g) (c.fn (imap g f x))
    }

eval :: forall a. Show a => Eq a => Env a -> Term a -> Eval (Value a)
eval env (Var v) = evalVar env v
eval _env (IntLit i) = pure $ VIntLit i
eval _env (BoolLit b) = pure $ VBoolLit b
eval env (Forall v srcTy bnd) = buildClosure env VForall v srcTy bnd
eval env (Exists v ty bnd) = buildClosure env VExists v ty bnd
eval env (Lam v ty bnd) = buildClosure env VLam v ty bnd
eval env (Op (Add x y)) = evalOp2 env (liftIntOp2 (+) VIntLit) NAdd x y
eval env (Op (Sub x y)) = evalOp2 env (liftIntOp2 (-) VIntLit) NSub x y
eval env (Op (Mul x y)) = evalOp2 env (liftIntOp2 (*) VIntLit) NMul x y
eval env (Op (Div x y)) = evalOp2 env (liftIntOp2 div VIntLit) NDiv x y
eval env (Op (Equal x y)) = evalOp2 env (liftIntOp2 (==) VBoolLit) NEqual x y
eval env (Op (Lt x y)) = evalOp2 env (liftIntOp2 (<) VBoolLit) NLt x y
eval env (Op (And x y)) = evalOp2 env (liftBoolOp2 (&&) VBoolLit) NAnd x y
eval env (Op (Or x y)) = evalOp2 env (liftBoolOp2 (||) VBoolLit) NOr x y
eval env (Not x) = evalOp env (liftBoolOp not VBoolLit) NNot x
eval env (App x y) = do
  xVal <- eval env x
  yVal <- eval env y
  evalApp xVal yVal
eval env (If ty x y z) = do
  xVal <- eval env x
  evalIf env ty xVal y z
eval env (The _ty x) = eval env x
eval _ BoolType = pure VBoolType
eval _ IntType = pure VIntType
eval _ (Universe k) = pure $ VUniverse k

evalApp :: forall a. Show a => Eq a =>
  Value a -> Value a -> Eval (Value a)
evalApp (VNeutral (VForall _srcTy resBnd) nX) y = do
  ty <- evalClosure resBnd y
  pure $ VNeutral ty $ NApp nX y
evalApp (VLam ty c) y = evalClosure c y
evalApp _ _ = error "evalApp"

evalIf :: forall a. Show a => Eq a =>
  Env a ->
  Type a ->
  Value a -> Term a -> Term a -> Eval (Value a)
evalIf env _ (VBoolLit true) y _ = eval env y
evalIf env _ (VBoolLit false) _ z = eval env z
evalIf env resTy (VNeutral ty n) y z =
  VNeutral <$> eval env resTy <*> (NIf n <$> eval env y <*> eval env z) -- TODO: Is this right?
evalIf _ _ _ _ _ = error "evalIf"

evalClosure :: forall a. Closure a -> Value a -> Eval (Value a)
evalClosure (Closure c) x = c.fn x

liftIntOp2 :: forall r a. (Int -> Int -> r) -> (r -> Value a) -> Value a -> Value a -> Value a
liftIntOp2 f g (VIntLit i) (VIntLit j) = g $ f i j
liftIntOp2 _ _ _ _ = error "liftIntOp2"

liftBoolOp :: forall r a. (Boolean -> r) -> (r -> Value a) -> Value a -> Value a
liftBoolOp f g (VBoolLit x) = g $ f x
liftBoolOp _ _ _ = error "liftBoolOp"

liftBoolOp2 :: forall r a. (Boolean -> Boolean -> r) -> (r -> Value a) -> Value a -> Value a -> Value a
liftBoolOp2 f g (VBoolLit x) (VBoolLit y) = g $ f x y
liftBoolOp2 _ _ _ _ = error "liftBoolOp2"

evalOp :: forall a. Show a => Eq a =>
  Env a ->
  (Value a -> Value a) ->
  (Neutral a -> Neutral a) ->
  Term a -> Eval (Value a)
evalOp env f g x = do
  x' <- eval env x
  case x' of
    VNeutral ty nX -> pure $ VNeutral ty $ g nX
    _ -> pure $ f x'


evalOp2 :: forall a. Show a => Eq a =>
  Env a ->
  (Value a -> Value a -> Value a) ->
  (Neutral a -> Value a -> Neutral a) ->
  Term a -> Term a -> Eval (Value a)
evalOp2 env f g x y = do
  x' <- eval env x
  y' <- eval env y
  case x' of
    VNeutral ty nX -> pure $ VNeutral ty $ g nX y'
    _ -> pure $ f x' y'

buildClosure :: forall a. Show a => Eq a =>
  Env a ->
  (TypeValue a -> Closure a -> Value a) ->
  String -> Type a -> CoreScope a -> Eval (Value a)
buildClosure env f v srcTy bnd = do
  let abstr :: Value a -> Eval (Value a)
      abstr = \x ->
          imap (imap go F) (imap F go) $ eval (convertEnv env v x) (fromScope bnd)
          -- imap (imap go F) (imap F go) $ eval (convertEnv env v x) (fromScope bnd)
  argType <- eval env srcTy
  pure $ f argType $ (Closure { argName: v, argType: argType, fn: abstr })
  where
    go :: forall c d. Var c d -> d
    go (B x) = error "buildClosure.go" -- TODO: Is this actually okay?
    go (F y) = y

evalVar :: forall a. Show a => Eq a => Env a -> a -> Eval (Value a)
evalVar env v =
  case find ((_ == v) <<< fst) env of
      Just (Tuple _ x) -> pure x
      Nothing -> evalError $ "Cannot find variable " <> show v

convertVar :: forall a. Show a => Eq a => Env a -> Value a ->
  Var CoreName a -> Eval (Value a)
convertVar _env val (B _) = pure val
convertVar env  _   (F x) = evalVar env x

convertEnv :: forall a. Show a => Env a -> String -> Value a -> Env (Var Unit a)
convertEnv env v val =
  Tuple (B unit) (imap F go val)
    :
  map (bimap F (imap F go)) env
  where
    go :: forall c d. Show c => Var c d -> d
    go (B x) = error "convertEnv.go" -- TODO: Is this actually okay?
    go (F y) = y

evalOpened :: forall a. Show a => Eq a => Env a -> Value a ->
  Term (Var CoreName a) -> Eval (Value a)
evalOpened env x = unsafeCoerce -- ?a <<< eval ?e
    -- go :: Var CoreName a -> Eval (Value a)
    -- go (F a) = evalVar env a
    -- go (B _) = pure x

evalScope :: forall a. Scope CoreName Term a -> Eval (Scope CoreName Value a)
evalScope = unsafeCoerce

-- extendEnv :: forall a. Env a -> a -> TypeValue a -> Env a
-- extendEnv = unsafeCoerce

type Eval = Either String

evalError :: forall a. String -> Eval a
evalError = Left

error :: forall a. String -> a
error = unsafePerformEffect <<< throw
