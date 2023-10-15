module Stratify.Utils
  ( charsToString
  , error
  )
  where

import Prelude

import Effect.Unsafe
import Effect.Exception

import Data.List
import Data.String.CodeUnits

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

charsToString :: List Char -> String
charsToString = fromCharArray <<< toUnfoldable

class Plate a where
    plate :: forall f. Applicative f => (a -> f a) -> a -> f a
