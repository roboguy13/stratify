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
import Data.Profunctor

import Unsafe.Coerce
import Effect.Unsafe
import Effect.Exception

data Value b a
  = VIntLit Int
  | VBoolLit Boolean
  | VLam (TypeValue b a) (Closure b a)
  | VForall (TypeValue b a) (Closure b a)
  | VExists (TypeValue b a) (Closure b a)
  | VIntType
  | VBoolType
  | VUniverse Int
  | VNeutral (TypeValue b a) (Neutral b a)

data Neutral b a
  = NVar a
  | NAdd (Neutral b a) (Normal b a)
  | NSub (Neutral b a) (Normal b a)
  | NMul (Neutral b a) (Normal b a)
  | NDiv (Neutral b a) (Normal b a)
  | NEqual (Neutral b a) (Normal b a)
  | NLt (Neutral b a) (Normal b a)
  | NAnd (Neutral b a) (Normal b a)
  | NOr (Neutral b a) (Normal b a)

  | NNot (Neutral b a)
  | NApp (Neutral b a) (Normal b a)
  | NIf (Neutral b a) (Normal b a) (Normal b a)

class ToVar a b where
  toVar :: a -> b

instance ToVar a b => ToVar (Var x a) (Var x b) where
  toVar = bimap identity toVar

instance Profunctor Value where
  dimap _ _ (VIntLit i) = VIntLit i
  dimap _ _ (VBoolLit b) = VBoolLit b
  dimap f g (VLam ty c) = VLam (dimap f g ty) (dimap f g c)
  dimap f g (VForall ty c) = VForall (dimap f g ty) (dimap f g c)
  dimap f g (VExists ty c) = VExists (dimap f g ty) (dimap f g c)
  dimap _ _ VIntType = VIntType
  dimap _ _ VBoolType = VBoolType
  dimap _ _ (VUniverse k) = VUniverse k
  dimap f g (VNeutral ty n) = VNeutral (dimap f g ty) (dimap f g n)

instance Profunctor Neutral where
  dimap _ g (NVar x) = NVar (g x)
  dimap f g (NAdd x y) = NAdd (dimap f g x) (dimap f g y)
  dimap f g (NSub x y) = NSub (dimap f g x) (dimap f g y)
  dimap f g (NMul x y) = NMul (dimap f g x) (dimap f g y)
  dimap f g (NEqual x y) = NEqual (dimap f g x) (dimap f g y)
  dimap f g (NDiv x y) = NDiv (dimap f g x) (dimap f g y)
  dimap f g (NLt x y) = NLt (dimap f g x) (dimap f g y)
  dimap f g (NAnd x y) = NAnd (dimap f g x) (dimap f g y)
  dimap f g (NOr x y) = NOr (dimap f g x) (dimap f g y)
  dimap f g (NNot x) = NNot (dimap f g x)
  dimap f g (NApp x y) = NApp (dimap f g x) (dimap f g y)
  dimap f g (NIf x y z) = NIf (dimap f g x) (dimap f g y) (dimap f g z)

type Normal = Value

-- data Normal a =
--   Normal
--   { ty :: TypeValue a
--   , value :: Value a
--   }

derive instance Generic (Value b a) _
derive instance Generic (Neutral b a) _
derive instance Generic (Closure b a) _
-- derive instance Generic (Normal a) _

instance (Show b, Show a) => Show (Value b a) where show x = genericShow x
instance (Show b, Show a) => Show (Neutral b a) where show x = genericShow x
instance Show (Closure b a) where show _ = "<closure>"
-- instance Show a => Show (Normal a) where show = genericShow

type TypeValue = Value

type Env b a = List (Tuple a (Value b a))

-- data Closure a =
--   Closure
--   { env :: Env a
--   , name :: String
--   , body :: Term a
--   }

data Closure b a =
  Closure
  { argName :: a
  , argType :: TypeValue b a
  , fn :: Value a b -> Eval (Value b a)
  }

instance Profunctor Closure where
  dimap f g (Closure c) =
    Closure
    { argName: g c.argName
    , argType: dimap f g c.argType
    , fn: dimap (dimap g f) (map (dimap f g)) c.fn
    }

type Eval = Either String

eval :: forall b a. ToVar a b => Show a => Eq a => Env b a -> Term' b a -> Eval (Value b a)
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

evalApp :: forall b a. ToVar a b => Show a => Eq a =>
  Value b a -> Value b a -> Eval (Value b a)
evalApp (VNeutral (VForall _srcTy resBnd) nX) y = do
  ty <- evalClosure resBnd y
  pure $ VNeutral ty $ NApp nX y
evalApp (VLam ty c) y = evalClosure c y
evalApp _ _ = error "evalApp"

evalIf :: forall b a. ToVar a b => Show a => Eq a =>
  Env b a ->
  Type' b a ->
  Value b a -> Term' b a -> Term' b a -> Eval (Value b a)
evalIf env _ (VBoolLit true) y _ = eval env y
evalIf env _ (VBoolLit false) _ z = eval env z
evalIf env resTy (VNeutral ty n) y z =
  VNeutral <$> eval env resTy <*> (NIf n <$> eval env y <*> eval env z) -- TODO: Is this right?
evalIf _ _ _ _ _ = error "evalIf"

evalClosure :: forall b a. ToVar a b => Closure b a -> Value b a -> Eval (Value b a)
evalClosure (Closure c) x = c.fn (dimap toVar toVar x)

liftIntOp2 :: forall r b a. (Int -> Int -> r) -> (r -> Value b a) -> Value b a -> Value b a -> Value b a
liftIntOp2 f g (VIntLit i) (VIntLit j) = g $ f i j
liftIntOp2 _ _ _ _ = error "liftIntOp2"

liftBoolOp :: forall r b a. (Boolean -> r) -> (r -> Value b a) -> Value b a -> Value b a
liftBoolOp f g (VBoolLit x) = g $ f x
liftBoolOp _ _ _ = error "liftBoolOp"

liftBoolOp2 :: forall r b a. (Boolean -> Boolean -> r) -> (r -> Value b a) -> Value b a -> Value b a -> Value b a
liftBoolOp2 f g (VBoolLit x) (VBoolLit y) = g $ f x y
liftBoolOp2 _ _ _ _ = error "liftBoolOp2"

evalOp :: forall b a. ToVar a b => Show a => Eq a =>
  Env b a ->
  (Value b a -> Value b a) ->
  (Neutral b a -> Neutral b a) ->
  Term' b a -> Eval (Value b a)
evalOp env f g x = do
  x' <- eval env x
  case x' of
    VNeutral ty nX -> pure $ VNeutral ty $ g nX
    _ -> pure $ f x'


evalOp2 :: forall b a. ToVar a b => Show a => Eq a =>
  Env b a ->
  (Value b a -> Value b a -> Value b a) ->
  (Neutral b a -> Value b a -> Neutral b a) ->
  Term' b a -> Term' b a -> Eval (Value b a)
evalOp2 env f g x y = do
  x' <- eval env x
  y' <- eval env y
  case x' of
    VNeutral ty nX -> pure $ VNeutral ty $ g nX y'
    _ -> pure $ f x' y'

buildClosure :: forall b a. ToVar a b => Show a => Eq a =>
  Env b a ->
  (TypeValue b a -> Closure b a -> Value b a) ->
  b -> Type' b a -> CoreScope b a -> Eval (Value b a)
buildClosure env f v srcTy bnd = do
  let abstr :: Value b a -> Eval (Value a b)
      abstr = \x ->
          ?a $ eval (convertEnv env v x) (fromScope bnd)
          -- imap (imap go F) (imap F go) $ eval (convertEnv env v x) (fromScope bnd)
          -- imap (imap go F) (imap F go) $ eval (convertEnv env v x) (fromScope bnd)
  argType <- eval env srcTy :: Eval (TypeValue b a)
  ?a
  -- pure $ f argType $ (Closure { argName: v, argType: argType, fn: ?abstr })
  where
    go :: forall c d. Var c d -> d
    go (B x) = error "buildClosure.go" -- TODO: Is this actually okay?
    go (F y) = y

evalVar :: forall b a. Show a => Eq a => Env b a -> a -> Eval (Value b a)
evalVar env v =
  case find ((_ == v) <<< fst) env of
      Just (Tuple _ x) -> pure x
      Nothing -> evalError $ "Cannot find variable " <> show v

convertVar :: forall b a. Show a => Eq a => Env b a -> Value b a ->
  Var CoreName a -> Eval (Value b a)
convertVar _env val (B _) = pure val
convertVar env  _   (F x) = evalVar env x

convertEnv :: forall b a. Show a => Env b a -> b -> Value b a -> Env b (Var Unit a)
convertEnv env v val = error "convertEnv"
  -- Tuple (B unit) (dimap F go val)
  --   :
  -- map (bimap F (imap F go)) env
  -- where
  --   go :: forall c d. Show c => Var c d -> d
  --   go (B x) = error "convertEnv.go" -- TODO: Is this actually okay?
  --   go (F y) = y

evalOpened :: forall b a. Show a => Eq a => Env b a -> Value b a ->
  Term (Var CoreName a) -> Eval (Value b a)
evalOpened env x = unsafeCoerce -- ?a <<< eval ?e
    -- go :: Var CoreName a -> Eval (Value a)
    -- go (F a) = evalVar env a
    -- go (B _) = pure x

evalScope :: forall b a. Scope CoreName (Term' b) a -> Eval (Scope CoreName (Value b) a)
evalScope = unsafeCoerce

-- extendEnv :: forall a. Env a -> a -> TypeValue a -> Env a
-- extendEnv = unsafeCoerce

evalError :: forall a. String -> Eval a
evalError = Left

error :: forall a. String -> a
error = unsafePerformEffect <<< throw
