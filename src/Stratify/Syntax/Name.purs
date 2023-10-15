-- See this comment by Edward Kmett: https://www.reddit.com/r/haskell/comments/j2q5p8/comment/g7zunsk/

module Stratify.Syntax.Name
  ( Ix
  , Level
  , Name
  , NameEnv
  , NamingCtx
  , NamingCtx'
  , emptyNameEnv
  , emptyNamingCtx
  , extend
  , initialLevel
  , ixHere
  , ixLevel
  , ixLookup
  , ixToName
  , liftNamingCtx
  , nameToIx
  , nextLevel
  , shiftIx
  )
  where

import Prelude

import Stratify.Utils

import Data.List
import Data.Maybe

import Data.Tuple
import Data.Generic.Rep
import Data.Show.Generic

type Name = String

newtype Level = Level Int
newtype Ix = Ix Int

derive instance Eq Level
derive instance Eq Ix

derive instance Generic Level _
derive instance Generic Ix _

instance Show Level where show = genericShow
instance Show Ix where show = genericShow

ixLevel :: Level -> Ix -> Level
ixLevel (Level depth) (Ix i) = Level (depth - i - 1)

newtype NameEnv a = NameEnv (List a)

ixLookup :: forall a. Ix -> NameEnv a -> Maybe a
ixLookup (Ix i) (NameEnv xs) = xs !! i

initialLevel :: Level
initialLevel = Level 0

ixHere :: Ix
ixHere = Ix 0

shiftIx :: Ix -> Ix
shiftIx (Ix i) = Ix (i + 1)

nextLevel :: Level -> Level
nextLevel (Level lvl) = Level (lvl + 1)

emptyNameEnv :: forall a. NameEnv a
emptyNameEnv = NameEnv Nil

extend :: forall a. a -> NameEnv a -> NameEnv a
extend x (NameEnv xs) = NameEnv (x : xs)

newtype NamingCtx' a = NamingCtx (List (Tuple Name a))

derive instance Functor NamingCtx'

type NamingCtx = NamingCtx' Ix

emptyNamingCtx :: forall a. NamingCtx' a
emptyNamingCtx = NamingCtx Nil

nameToIx :: NamingCtx -> Name -> Ix
nameToIx (NamingCtx nCtx) n =
  case find ((_ == n) <<< fst) nCtx of
    Just (Tuple _ i) -> i
    Nothing -> error "nameToIx"

ixToName :: NamingCtx -> Ix -> Name
ixToName (NamingCtx nCtx) i =
  case find ((_ == i) <<< snd) nCtx of
    Just (Tuple n _) -> n
    Nothing -> error "ixToName"

-- | For when we go under a binder
liftNamingCtx :: Name -> NamingCtx -> NamingCtx
liftNamingCtx n nCtx =
  let NamingCtx nCtx' = map shiftIx nCtx
  in
  NamingCtx $
    Tuple n ixHere
      :
    nCtx'

-- -- See Stephanie Weirich's talk: https://www.youtube.com/watch?v=j2xYSxMkXeQ
-- type Subst = NameEnv

-- class Substitute a where
--   var :: Ix -> a
--   subst :: Subst a -> a -> a
-- -- applySubst :: Subst a -> Ix -> a

-- oneSubst :: forall a. a -> Subst a
-- oneSubst x = NameEnv $ x : Nil

-- -- shift :: forall a. Subst a -> Subst a
-- -- shift (NameEnv xs) = ?a


-- -- levelIx :: Ix -> Level -> Ix

-- -- class DeBruijn a where
