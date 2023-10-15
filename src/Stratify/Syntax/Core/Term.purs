module Stratify.Syntax.Core.Term
  where

import Bound
import Control.Apply
import Data.Eq
import Data.Generic.Rep
import Data.List
import Data.Maybe
import Data.Show
import Data.Show.Generic
import Data.Traversable
import Data.Tuple
import Prelude
import Prim hiding (Type)
import Data.Monoid

import Stratify.Syntax.Name
import Stratify.Pretty.Doc
import Stratify.Ppr

import Data.Foldable (fold)

-- data Type a
--   = IntType
--   | TyVar a
--   | BoolType
--   | FnType (Type a) (Type a)

-- | NOTE: These are indistinguishable by (==), (<=), compare, etc. They are intended to be distinguished by type nesting structure.
type CoreName = Name String Unit

mkCoreName :: String -> CoreName
mkCoreName s = Name { base: s, unique: unit }

-- type CoreScope a = Scope CoreName Term a
type CoreScope b a = Scope Unit (Term' b) a

data Op' b a
  = Add (Term' b a) (Term' b a)
  | Sub (Term' b a) (Term' b a)
  | Mul (Term' b a) (Term' b a)
  | Div (Term' b a) (Term' b a)
  | Equal (Term' b a) (Term' b a)
  | Lt (Term' b a) (Term' b a)
  | And (Term' b a) (Term' b a)
  | Or (Term' b a) (Term' b a)

type Op a = Op' a a

data Term' :: Prim.Type -> Prim.Type -> Prim.Type
data Term' b a
  = Var a
  | IntLit Int
  | BoolLit Boolean
  | Op (Op' b a)
  | Not (Term' b a)
  | App (Term' b a) (Term' b a)
  | Lam b (Type' b a) (CoreScope b a)
  | If (Type' b a) (Term' b a) (Term' b a) (Term' b a)

  | The (Type' b a) (Term' b a)

  -- | Ann (Term a) (Type a)

  -- | Y (Scope String Term a)

  -- | Match (Term a) (List (MatchBranch a))

  -- Types --
  | Forall b (Type' b a) (CoreScope b a)
  | Exists b (Type' b a) (CoreScope b a)
  | IntType
  | BoolType
  | Universe Int
  -- | Prop -- Impredicative universe of propositions

type Term :: Prim.Type -> Prim.Type
type Term a = Term' a a
type Type' = Term'
type Type :: Prim.Type -> Prim.Type
type Type a = Term a

-- Basic pretty-printer. Other pretty-printers should be used for particular language levels
instance (Ppr a) => Ppr (Term' a a) where
  pprDoc (Var v) = pprDoc v
  pprDoc (IntLit i) = text $ show i
  pprDoc (BoolLit b) = text $ show b
  pprDoc (Op x) = pprDoc x
  pprDoc (Not x) = pprNested x
  pprDoc (App x y) = pprNested x <+> pprNested y
  pprDoc (Lam x ty bnd) = text "\\" <> parens (pprDoc x <+> text ":" <+> pprDoc ty) <> text "." <+> pprDoc (instantiate1 (Var x) bnd)
  pprDoc (If ty x y z) = text "if" <+> pprDoc x <+> text "then" <+> pprDoc y <+> text "else" <+> pprDoc y
  pprDoc (The ty x) = text "the" <+> pprNested ty <+> pprNested x
  pprDoc (Forall x ty bnd) = text "forall" <+> parens (pprDoc x <+> text ":" <+> pprDoc ty) <> text "." <+> pprDoc (instantiate1 (Var x) bnd)
  pprDoc (Exists x ty bnd) = text "exists" <+> parens (pprDoc x <+> text ":" <+> pprDoc ty) <> text "." <+> pprDoc (instantiate1 (Var x) bnd)
  pprDoc IntType = text "Int"
  pprDoc BoolType = text "Bool"
  pprDoc (Universe 0) = text "Type"
  pprDoc (Universe k) = text "Type" <+> text (show k)

instance Ppr a => Nested (Term' a a) where
  isNested (Var _) = false
  isNested (IntLit _) = false
  isNested (BoolLit _) = false
  isNested (Op _) = true
  isNested (App x y) = true
  isNested (Lam _ _ _) = true
  isNested (If _ _ _ _) = true
  isNested (The _ _) = true
  isNested (Universe _) = true
  isNested (Forall _ _ _) = true
  isNested (Exists _ _ _) = true
  isNested (Not _) = true
  isNested IntType = false
  isNested BoolType = false

instance Ppr a => Ppr (Op a) where
  pprDoc e = case e of
      Add x y -> pprBin "+" x y
      Sub x y -> pprBin "-" x y
      Mul x y -> pprBin "*" x y
      Div x y -> pprBin "/" x y
      Equal x y -> pprBin "==" x y
      Lt x y -> pprBin "<" x y
      And x y -> pprBin "&&" x y
      Or x y -> pprBin "||" x y
    where
      pprBin op x y = pprDoc x <+> text op <+> pprDoc y

fnType :: forall b a. Monoid b => Eq a => Monoid a => Type' b a -> Type' b a -> Type' b a
fnType src tgt = Forall mempty src (abstract1 mempty tgt)

lam :: String -> Type String -> Term String -> Term String
lam x ty body = Lam x ty (abstract1 x body)

type MatchBranch b a =
  { constructor :: String
  , scope :: Scope Unit (Term' b) (List a)
  }


type OpenedVar b a = Var (Name String Unit) (Term' b a)
openTerm :: forall b a. Term' b a -> Term' b (OpenedVar b a)
openTerm = map (F <<< Var)

-- instance Unify Term where
--   isVar (Var x) = Just x
--   isVar _ = Nothing
--
--   mkVar = Var
--
--   match (IntLit i) (IntLit j)
--     | i == j = Just Nil
--     | otherwise = Nothing
--
--   match (BoolLit b) (BoolLit b')
--     | b == b' = Just Nil
--     | otherwise = Nothing
--
--   match (Op x) (Op y) = matchOp x y
--   match (Not x) (Not x') = Just (Tuple x x' : Nil)
--   match (App x y) (App x' y') = Just (Tuple x x' : Tuple y y' : Nil)
--   -- match (Lam x
--
--   children = ?a
--   freeVars = ?a
--
-- matchOp :: forall a. Op a -> Op a -> Maybe (List (Tuple (Term a) (Term a)))
-- matchOp (Add x y) (Add x' y') = Just (Tuple x x' : Tuple y y' : Nil)
-- matchOp (Sub x y) (Sub x' y') = Just (Tuple x x' : Tuple y y' : Nil)
-- matchOp (Mul x y) (Mul x' y') = Just (Tuple x x' : Tuple y y' : Nil)
-- matchOp (Div x y) (Div x' y') = Just (Tuple x x' : Tuple y y' : Nil)
-- matchOp (Equal x y) (Equal x' y') = Just (Tuple x x' : Tuple y y' : Nil)
-- matchOp (Lt x y) (Lt x' y') = Just (Tuple x x' : Tuple y y' : Nil)
-- matchOp (And x y) (And x' y') = Just (Tuple x x' : Tuple y y' : Nil)
-- matchOp (Or x y) (Or x' y') = Just (Tuple x x' : Tuple y y' : Nil)
-- matchOp _ _ = Nothing

derive instance functorOp :: Functor (Op' b)
derive instance functorTerm :: Functor (Term' b)
derive instance eqOp :: (Eq b, Eq a) => Eq (Op' b a)
derive instance eqTerm :: (Eq b, Eq a) => Eq (Term' b a)
derive instance eq1Term :: Eq b => Eq1 (Term' b)

instance applyTerm :: Apply (Term' b) where
  apply = ap

instance applicativeTerm :: Applicative (Term' b) where
  pure = Var

instance bindTerm :: Bind (Term' b) where
  bind t f =
    case t of
      Op x0 ->
        Op $ case x0 of
                 Add x y -> Add (bind x f) (bind y f)
                 Sub x y -> Sub (bind x f) (bind y f)
                 Mul x y -> Mul (bind x f) (bind y f)
                 Div x y -> Div (bind x f) (bind y f)
                 Equal x y -> Equal (bind x f) (bind y f)
                 Lt x y -> Lt (bind x f) (bind y f)
                 And x y -> And (bind x f) (bind y f)
                 Or x y -> Or (bind x f) (bind y f)

      Var x -> f x
      IntLit i -> IntLit i
      BoolLit b -> BoolLit b
      Not x -> Not (bind x f)

      App x y -> App (bind x f) (bind y f)
      Lam v ty bnd -> Lam v (bind ty f) (bnd >>>= f)
      If ty x y z -> If (bind ty f) (bind x f) (bind y f) (bind z f)
      Forall v ty bnd -> Forall v (bind ty f) (bnd >>>= f)
      Exists v ty bnd -> Exists v (bind ty f) (bnd >>>= f)
      IntType -> IntType
      BoolType -> BoolType
      Universe k -> Universe k

      The ty x -> The (bind ty f) (bind x f)

instance monadTerm :: Monad (Term' b)

freeVars :: forall b a. Term' b a -> List a
freeVars (Var v) = v : Nil
freeVars (Op (Add x y)) = freeVars x <> freeVars y
freeVars (Op (Sub x y)) = freeVars x <> freeVars y
freeVars (Op (Mul x y)) = freeVars x <> freeVars y
freeVars (Op (Div x y)) = freeVars x <> freeVars y
freeVars (Op (Equal x y)) = freeVars x <> freeVars y
freeVars (Op (Lt x y)) = freeVars x <> freeVars y
freeVars (Op (And x y)) = freeVars x <> freeVars y
freeVars (Op (Or x y)) = freeVars x <> freeVars y
freeVars (IntLit _) = Nil
freeVars (BoolLit _) = Nil
freeVars (Not x) = freeVars x
freeVars (App x y) = freeVars x <> freeVars y
freeVars (If ty x y z) = freeVars ty <> freeVars x <> freeVars y <> freeVars z
freeVars IntType = Nil
freeVars BoolType = Nil
freeVars (Universe _) = Nil
freeVars (Lam _ ty bnd) = freeVars ty <> (mapMaybe toF $ freeVars $ fromScope bnd)
freeVars (Forall _ ty bnd) = freeVars ty <> mapMaybe toF (freeVars (fromScope bnd))
freeVars (Exists _ ty bnd) = freeVars ty <> mapMaybe toF (freeVars (fromScope bnd))
freeVars (The ty t) = freeVars ty <> freeVars t

toF :: forall b a. Var b a -> Maybe a
toF (F x) = Just x
toF (B _) = Nothing


-- instance foldableTerm :: Foldable (Term' b) where
--   foldMap f = fold <<< freeVars <<< map f
--   foldr f = foldrDefault f
--   foldl f = foldlDefault f

-- instance traversableTerm :: Traversable (Term' b) where
--   traverse f (Var x) = Var <$> f x
--   traverse f (Op (Add x y)) = Op <$> lift2 Add (traverse f x) (traverse f y)
--   traverse f (Op (Sub x y)) = Op <$> lift2 Sub (traverse f x) (traverse f y)
--   traverse f (Op (Mul x y)) = Op <$> lift2 Mul (traverse f x) (traverse f y)
--   traverse f (Op (Div x y)) = Op <$> lift2 Div (traverse f x) (traverse f y)
--   traverse f (Op (Equal x y)) = Op <$> lift2 Equal (traverse f x) (traverse f y)
--   traverse f (Op (Lt x y)) = Op <$> lift2 Lt (traverse f x) (traverse f y)
--   traverse f (Op (And x y)) = Op <$> lift2 And (traverse f x) (traverse f y)
--   traverse f (Op (Or x y)) = Op <$> lift2 Or (traverse f x) (traverse f y)
--   traverse f (IntLit i) = pure $ IntLit i
--   traverse f (BoolLit b) = pure $ BoolLit b
--   traverse f (Not x) = map Not (traverse f x)
--   traverse f (App x y) = lift2 App (traverse f x) (traverse f y)
--   traverse f (If ty x y z) = lift4 If (traverse f ty) (traverse f x) (traverse f y) (traverse f z)
--   traverse f IntType = pure IntType
--   traverse f BoolType = pure BoolType
--   traverse f (Universe k) = pure $ Universe k
--   traverse f (Lam v ty bnd) = Lam v <$> traverse f ty <*> traverse f bnd
--   traverse f (Forall v ty bnd) = Forall v <$> traverse f ty <*> (traverse f bnd)
--   traverse f (Exists v ty bnd) = Exists v <$> traverse f ty <*> (traverse f bnd)
--   traverse f (The ty t) = The <$> traverse f ty <*> traverse f t

--   sequence = traverse identity

-- derive instance genericTerm :: Generic (Term' b a) _
-- derive instance genericOp :: Generic (Op' b a) _

-- instance showTerm :: Show a => Show (Term' b a) where
--   show = genericShow

-- instance showOp :: Show a => Show (Op' b a) where
--   show = genericShow

