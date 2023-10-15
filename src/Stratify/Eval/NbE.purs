-- Normalization-by-Evaluation
module Stratify.Eval.NbE
  where

import Prim hiding (Type)
import Prelude

import Stratify.Syntax.Core.Term
import Stratify.Syntax.Name
import Stratify.Ppr
import Stratify.Utils

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

data Value
  = VIntLit Int
  | VBoolLit Boolean
  | VLam TypeValue Closure
  | VForall TypeValue Closure
  | VExists TypeValue Closure
  | VIntType
  | VBoolType
  | VUniverse Int
  | VNeutral Neutral

data Neutral
  = NVar Name Level
  | NAdd Neutral Value
  | NSub Neutral Value
  | NMul Neutral Value
  | NDiv Neutral Value
  | NEqual Neutral Value
  | NLt Neutral Value
  | NAnd Neutral Value
  | NOr Neutral Value

  | NNot Neutral
  | NApp Neutral Value
  | NIf TypeValue Neutral Value Value

type TypeValue = Value

data Closure =
  Closure
  { argName :: Name
  , fn :: Value -> Eval Value
  }

derive instance Generic Value _
derive instance Generic Neutral _

instance Show Value where show x = genericShow x
instance Show Neutral where show x = genericShow x
instance Show Closure where show _ = "<closure>"

type Eval = Either String

type Env = NameEnv Value

instance Ppr Value where
  pprDoc x =
    case quote initialLevel x of
      Left e -> error e
      Right r -> pprDoc r

nf :: Term -> Eval Term
nf = quote initialLevel <=< eval emptyNameEnv

quote :: Level -> Value -> Eval Term
quote depth (VIntLit i) = pure $ IntLit i
quote depth (VBoolLit b) = pure $ BoolLit b
quote depth (VLam ty c0@(Closure c)) =
  Lam c.argName
    <$> quote (nextLevel depth) ty
    <*> quoteAbs (nextLevel depth) c0
quote depth (VForall ty c0@(Closure c)) =
  Forall c.argName
    <$> quote (nextLevel depth) ty
    <*> quoteAbs (nextLevel depth) c0
quote depth (VExists ty c0@(Closure c)) =
  Exists c.argName
    <$> quote (nextLevel depth) ty
    <*> quoteAbs (nextLevel depth) c0
quote depth VIntType = pure IntType
quote depth VBoolType = pure BoolType
quote depth (VUniverse k) = pure $ Universe k
quote depth (VNeutral n) = quoteNeutral depth n

quoteNeutral :: Level -> Neutral -> Eval Term
quoteNeutral depth (NVar x lvl) = pure $ Var (IxName x (levelIx depth lvl)) -- TODO: Does this work?
quoteNeutral depth (NAdd x y) = Op <$> (Add <$> quoteNeutral depth x <*> quote depth y)
quoteNeutral depth (NSub x y) = Op <$> (Sub <$> quoteNeutral depth x <*> quote depth y)
quoteNeutral depth (NMul x y) = Op <$> (Mul <$> quoteNeutral depth x <*> quote depth y)
quoteNeutral depth (NDiv x y) = Op <$> (Div <$> quoteNeutral depth x <*> quote depth y)
quoteNeutral depth (NEqual x y) = Op <$> (Equal <$> quoteNeutral depth x <*> quote depth y)
quoteNeutral depth (NLt x y) = Op <$> (Lt <$> quoteNeutral depth x <*> quote depth y)
quoteNeutral depth (NAnd x y) = Op <$> (And <$> quoteNeutral depth x <*> quote depth y)
quoteNeutral depth (NOr x y) = Op <$> (Or <$> quoteNeutral depth x <*> quote depth y)
quoteNeutral depth (NNot x) = Not <$> quoteNeutral depth x
quoteNeutral depth (NApp x y) = App <$> quoteNeutral depth x <*> quote depth y
quoteNeutral depth (NIf ty x y z) =
  If <$> quote depth ty <*> quoteNeutral depth x <*> quote depth y <*> quote depth z

quoteAbs :: Level -> Closure -> Eval Term
quoteAbs lvl (Closure c) = quote lvl =<< (c.fn (VNeutral (NVar c.argName lvl)))

eval :: Env -> Term -> Eval Value
eval env (Var (IxName x i)) =
  case ixLookup i env of
    Just r -> pure r
    Nothing -> Left $ "Cannot find variable " <> show x
eval env (IntLit i) = pure $ VIntLit i
eval env (BoolLit b) = pure $ VBoolLit b
eval env (Op x) = evalOp env x
eval env (Not x) = liftBoolOp not VBoolLit NNot <$> eval env x
eval env (App x y) = do
  xVal <- eval env x
  yVal <- eval env y
  evalApp xVal yVal
eval env (Lam v ty body) = VLam <$> eval env ty <*> pure (mkClosure env v body)
eval env (If ty x y z) = do
  tyVal <- eval env ty
  xVal <- eval env x
  evalIf env tyVal xVal y z
eval env (The _ty x) = eval env x
eval env (Forall v ty body) = VForall <$> eval env ty <*> pure (mkClosure env v body)
eval env (Exists v ty body) = VExists <$> eval env ty <*> pure (mkClosure env v body)
eval env IntType = pure VIntType
eval env BoolType = pure VBoolType
eval env (Universe k) = pure (VUniverse k)

evalOp :: Env -> Op -> Eval Value
evalOp env (Add x y) = liftIntOp2 (+) VIntLit NAdd <$> eval env x <*> eval env y
evalOp env (Sub x y) = liftIntOp2 (-) VIntLit NSub <$> eval env x <*> eval env y
evalOp env (Mul x y) = liftIntOp2 (*) VIntLit NMul <$> eval env x <*> eval env y
evalOp env (Div x y) = liftIntOp2 div VIntLit NDiv <$> eval env x <*> eval env y
evalOp env (Equal x y) = liftIntOp2 (==) VBoolLit NEqual <$> eval env x <*> eval env y
evalOp env (Lt x y) = liftIntOp2 (<) VBoolLit NLt <$> eval env x <*> eval env y
evalOp env (And x y) = liftBoolOp2 (&&) VBoolLit NAnd <$> eval env x <*> eval env y
evalOp env (Or x y) = liftBoolOp2 (||) VBoolLit NOr <$> eval env x <*> eval env y

mkClosure :: Env -> Name -> Term -> Closure
mkClosure env x body =
  Closure
  { argName: x
  , fn: \arg -> eval (extend arg env) body
  }

evalClosure :: Closure -> Value -> Eval Value
evalClosure (Closure c) x = c.fn x

evalApp :: Value -> Value -> Eval Value
evalApp (VNeutral nX) y = do
  pure $ VNeutral $ NApp nX y
evalApp (VLam ty c) y = evalClosure c y
evalApp _ _ = error "evalApp"

evalIf ::
  Env ->
  TypeValue -> Value -> Term -> Term -> Eval Value
evalIf env tyVal (VBoolLit true) y _ = eval env y
evalIf env tyVal (VBoolLit false) _ z = eval env z
evalIf env tyVal (VNeutral n) y z =
  VNeutral <$> (NIf tyVal n <$> eval env y <*> eval env z) -- TODO: Is this right in terms of evaluation order?
evalIf _ _ _ _ _ = error "evalIf"

liftIntOp2 :: forall r. (Int -> Int -> r) -> (r -> Value) -> (Neutral -> Value -> Neutral) -> Value -> Value -> Value
liftIntOp2 f g h (VIntLit i) (VIntLit j) = g $ f i j
liftIntOp2 f g h (VNeutral x) y = VNeutral $ h x y
liftIntOp2 _ _ _ _ _ = error "liftIntOp2"

liftBoolOp :: forall r. (Boolean -> r) -> (r -> Value) -> (Neutral -> Neutral) -> Value -> Value
liftBoolOp f g _ (VBoolLit x) = g $ f x
liftBoolOp f g h (VNeutral x) = VNeutral $ h x
liftBoolOp _ _ _ _ = error "liftBoolOp"

liftBoolOp2 :: forall r. (Boolean -> Boolean -> r) -> (r -> Value) -> (Neutral -> Value -> Neutral) -> Value -> Value -> Value
liftBoolOp2 f g h (VBoolLit x) (VBoolLit y) = g $ f x y
liftBoolOp2 f g h (VNeutral x) y = VNeutral $ h x y
liftBoolOp2 _ _ _ _ _ = error "liftBoolOp2"
