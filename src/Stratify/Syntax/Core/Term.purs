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
import Data.String as String
import Data.Bifunctor

import Stratify.Syntax.Name
import Stratify.Pretty.Doc
import Stratify.Ppr
import Stratify.Utils

import Data.Foldable (fold)

-- data Type a
--   = IntType
--   | TyVar a
--   | BoolType
--   | FnType (Type a) (Type a)

data Op'' a b
  = Add (Term'' a b) (Term'' a b)
  | Sub (Term'' a b) (Term'' a b)
  | Mul (Term'' a b) (Term'' a b)
  | Div (Term'' a b) (Term'' a b)
  | Equal (Term'' a b) (Term'' a b)
  | Lt (Term'' a b) (Term'' a b)
  | And (Term'' a b) (Term'' a b)
  | Or (Term'' a b) (Term'' a b)

-- TODO: Make the Boolean type just be a sum type (after I add sum types)
data Term'' b a
  = Var a
  | IntLit Int
  | BoolLit Boolean
  | Op (Op'' b a)
  | Not (Term'' b a)
  | App (Term'' b a) (Term'' b a)
  | Lam b (Type'' b a) (Term'' b a)
  | If (Term'' b a) (Term'' b a) (Term'' b a)

  | The (Type'' b a) (Term'' b a)

  -- | Ann ((Term'' b a) a) ((Type' a) a)

  -- | Y (Scope String (Term'' b a) a)

  -- | Match ((Term'' b a) a) (List (MatchBranch a))

  -- (Type' a)s --
  | Forall b (Type'' b a) (Term'' b a)
  | Exists b (Type'' b a) (Term'' b a)
  | IntType
  | BoolType
  | Universe Int
  -- | Prop -- Impredicative universe of propositions


derive instance Generic (Term'' b a) _
derive instance Generic (Op'' b a) _

instance showTerm :: (Show b, Show a) => Show (Term'' b a) where
  show x = genericShow x

instance showOp :: (Show b, Show a) => Show (Op'' b a) where
  show x = genericShow x

type Term' a = Term'' Name a
type Op' = Op'' Name

type Type'' = Term''
type Type' a = Term' a
type Type = Term
type Op = Op' IxName

data IxName =
  IxName String Ix
  -- { name :: String
  -- , ix :: Ix
  -- }

instance Ppr IxName where
  pprDoc (IxName x i) = text x <> text "@" <> pprDoc i

instance Eq IxName where
  eq (IxName _ i) (IxName _ j) = i == j

instance HasIx IxName where
  getIx (IxName _ i) = i

instance HasVar IxName where
  isVar (IxName _ i) = Just i

instance MkVar (Term'' b) IxName where
  mkVar = Var

derive instance (Eq b, Eq a) => Eq (Term'' b a)
derive instance (Eq b, Eq a) => Eq (Op'' b a)

derive instance Generic IxName _
instance Show IxName where show = genericShow

shiftIxName :: IxName -> IxName
shiftIxName (IxName x i) =
  IxName x (shiftIx i)
  -- { name: x.name
  -- , ix: shiftIx x.ix
  -- }

type Term = Term' IxName
type SurfaceTerm = Term'' String String
type SurfaceOp = Op'' String String
type SurfaceType = Type'' String String

instance IsName IxName where
  mkWildcardName = IxName "" ixHere
  isWildcardName (IxName "" _) = true -- TODO: Is this right?
  isWildcardName _ = false

isBinderHereUnused :: forall b a. HasIx a => Term'' b a -> Boolean
isBinderHereUnused = go ixHere
  where
    go :: Ix -> Term'' b a -> Boolean
    go i (Var x) = getIx x /= i
    go i (IntLit _) = true
    go i (BoolLit _) = true
    go i (Not x) = go i x
    go i (App x y) = go i x && go i y
    go i (If x y z) = go i x && go i y && go i z
    go i (Lam x ty body) = go i ty && go (shiftIx i) body
    go i (The ty x) = go i ty && go i x
    go i (Forall x ty body) = go i ty && go (shiftIx i) body
    go i (Exists x ty body) = go i ty && go (shiftIx i) body
    go i IntType = true
    go i BoolType = true
    go i (Universe k) = true
    go i (Op (Add x y)) = go i x && go i y
    go i (Op (Sub x y)) = go i x && go i y
    go i (Op (Mul x y)) = go i x && go i y
    go i (Op (Div x y)) = go i x && go i y
    go i (Op (Equal x y)) = go i x && go i y
    go i (Op (Lt x y)) = go i x && go i y
    go i (Op (And x y)) = go i x && go i y
    go i (Op (Or x y)) = go i x && go i y

derive instance Functor (Term'' b)
derive instance Functor (Op'' b)

derive instance Bifunctor Term''
derive instance Bifunctor Op''

instance Apply (Term'' b) where
  apply = ap

instance Applicative (Term'' b) where
  pure = Var

instance Bind (Term'' b) where
  bind (Var x) f = f x
  bind (IntLit i) _ = IntLit i
  bind (BoolLit b) _ = BoolLit b
  bind (Not x) f = Not (x >>= f)
  bind (App x y) f = App (x >>= f) (y >>= f)
  bind (If x y z) f = If (x >>= f) (y >>= f) (z >>= f)
  bind (Lam x ty body) f = Lam x (ty >>= f) (body >>= f)
  bind (The ty x) f = The (ty >>= f) (x >>= f)
  bind (Forall x ty body) f = Forall x (ty >>= f) (body >>= f)
  bind (Exists x ty body) f = Exists x (ty >>= f) (body >>= f)
  bind IntType _ = IntType
  bind BoolType _ = BoolType
  bind (Universe k) _ = Universe k
  bind (Op (Add x y)) f = Op (Add (x >>= f) (y >>= f))
  bind (Op (Sub x y)) f = Op (Sub (x >>= f) (y >>= f))
  bind (Op (Mul x y)) f = Op (Mul (x >>= f) (y >>= f))
  bind (Op (Div x y)) f = Op (Div (x >>= f) (y >>= f))
  bind (Op (Equal x y)) f = Op (Equal (x >>= f) (y >>= f))
  bind (Op (Lt x y)) f = Op (Lt (x >>= f) (y >>= f))
  bind (Op (And x y)) f = Op (And (x >>= f) (y >>= f))
  bind (Op (Or x y)) f = Op (Or (x >>= f) (y >>= f))

instance Monad (Term'' b)

-- termPlate :: forall f a b. Applicative f =>
--   (Term' a -> f (Term' b)) -> Term' a -> f (Term' b)
-- termPlate f x (Var x) = ?a

overOp :: forall x y a b. (Term'' x a -> Term'' y b) -> Op'' x a -> Op'' y b
overOp f (Add x y) = Add (f x) (f y)
overOp f (Sub x y) = Sub (f x) (f y)
overOp f (Mul x y) = Mul (f x) (f y)
overOp f (Div x y) = Div (f x) (f y)
overOp f (Equal x y) = Equal (f x) (f y)
overOp f (Lt x y) = Lt (f x) (f y)
overOp f (And x y) = And (f x) (f y)
overOp f (Or x y) = Or (f x) (f y)

fromNamed :: SurfaceTerm -> Term
fromNamed = go emptyNamingCtx
  where
    go :: NamingCtx -> SurfaceTerm -> Term
    go nCtx (Var x) = Var (IxName x (nameToIx nCtx (Name x)))
    go nCtx (IntLit i) = IntLit i
    go nCtx (BoolLit b) = BoolLit b
    go nCtx (Op x) = Op $ goOp nCtx x
    go nCtx (Not x) = Not $ go nCtx x
    go nCtx (App x y) = App (go nCtx x) (go nCtx y)
    go nCtx (Lam x ty body) =
      goAbstraction nCtx Lam x ty body
    go nCtx (If x y z) =
      If (go nCtx x) (go nCtx y) (go nCtx z)
    go nCtx (The ty x) =
      The (go nCtx ty) (go nCtx x)
    go nCtx (Forall x ty body) =
      goAbstraction nCtx Forall x ty body
    go nCtx (Exists x ty body) =
      goAbstraction nCtx Exists x ty body
    go nCtx IntType = IntType
    go nCtx BoolType = BoolType
    go nCtx (Universe k) = Universe k

    goOp nCtx = overOp (go nCtx)

    goAbstraction ::
      NamingCtx ->
      (Name -> Type -> Term -> Term) ->
      String -> SurfaceTerm -> SurfaceTerm -> Term
    goAbstraction nCtx f x ty body =
      let nCtx' = liftNamingCtx (Name x) nCtx
      in
      f (Name x) (go nCtx' ty) (go nCtx' body)

toNamed :: Term -> SurfaceTerm
toNamed = bimap unName go
  where
    go :: IxName -> String
    go (IxName n _) = n

-- Basic pretty-printer. Other pretty-printers should be used for particular language levels
instance (IsName b, HasIx a, IsName a, Ppr b, Ppr a) => Ppr (Term'' b a) where
  pprDoc (Var v) = pprDoc v
  pprDoc (IntLit i) = text $ show i
  pprDoc (BoolLit b) = text $ show b
  pprDoc (Op x) = pprDoc x
  pprDoc (Not x) = pprNested x
  pprDoc (App x y) = pprNested x <+> pprNested y
  pprDoc (Lam x ty body) = text "\\" <> parens (pprDoc x <+> text ":" <+> pprDoc ty) <> text "." <+> pprDoc body
  pprDoc (If x y z) = text "if" <+> pprDoc x <+> text "then" <+> pprDoc y <+> text "else" <+> pprDoc z
  pprDoc (The ty x) = text "the" <+> pprNested ty <+> pprNested x
  pprDoc (Forall x ty body) =
    if isBinderHereUnused body
    then pprDoc ty <+> text "->" <+> pprDoc body
    else text "forall" <+> parens (pprDoc x <+> text ":" <+> pprDoc ty) <> text "." <+> pprDoc body
  pprDoc (Exists x ty body) = text "exists" <+> parens (pprDoc x <+> text ":" <+> pprDoc ty) <> text "." <+> pprDoc body
  pprDoc IntType = text "Int"
  pprDoc BoolType = text "Bool"
  pprDoc (Universe 0) = text "Type"
  pprDoc (Universe k) = text "Type" <+> text (show k)

instance (IsName b, HasIx a, IsName a, Ppr b, Ppr a) => Nested (Term'' b a) where
  isNested (Var _) = false
  isNested (IntLit _) = false
  isNested (BoolLit _) = false
  isNested (Op _) = true
  isNested (App _ _) = true
  isNested (Lam _ _ _) = true
  isNested (If _ _ _) = true
  isNested (The _ _) = true
  isNested (Universe _) = true
  isNested (Forall _ _ _) = true
  isNested (Exists _ _ _) = true
  isNested (Not _) = true
  isNested IntType = false
  isNested BoolType = false

instance (IsName a, HasIx a, IsName b, Ppr b, Ppr a) => Ppr (Op'' b a) where
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

fnType :: forall b a. IsName b => Type'' b a -> Type'' b a -> Type'' b a
fnType = Forall mkWildcardName
