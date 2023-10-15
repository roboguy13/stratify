module Stratify.Utils where

import Prelude

import Effect.Unsafe
import Effect.Exception

error :: forall a. String -> a
error = unsafePerformEffect <<< throw

class Plate a where
    plate :: forall f. Applicative f => (a -> f a) -> a -> f a
