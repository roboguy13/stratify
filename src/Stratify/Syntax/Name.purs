-- See this comment by Edward Kmett: https://www.reddit.com/r/haskell/comments/j2q5p8/comment/g7zunsk/

module Stratify.Syntax.Name
  ( Ix
  , Level
  , Name
  , NameEnv
  , extend
  , ixLevel
  , ixLookup
  )
  where

import Prelude

import Data.List
import Data.Maybe

type Name = String

newtype Level = Level Int
newtype Ix = Ix Int

ixLevel :: Level -> Ix -> Level
ixLevel (Level depth) (Ix i) = Level (depth - i - 1)

newtype NameEnv a = NameEnv (List a)

ixLookup :: forall a. Ix -> NameEnv a -> Maybe a
ixLookup (Ix i) (NameEnv xs) = xs !! i

extend :: forall a. a -> NameEnv a -> NameEnv a
extend x (NameEnv xs) = NameEnv (x : xs)

-- levelIx :: Ix -> Level -> Ix

-- class DeBruijn a where
