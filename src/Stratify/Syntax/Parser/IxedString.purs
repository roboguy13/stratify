module Stratify.Syntax.Parser.IxedString where

import Prelude

import Data.Array ((!!), null)
import Data.String.CodeUnits
import Data.Maybe
import Data.Either

import Data.Generic.Rep
import Data.Show.Generic

data IxedString =
    IxedString
    { string :: Array Char
    , ix :: Int
    , len :: Int
    }

derive instance Generic IxedString _
instance Show IxedString where show = genericShow

mkIxedString :: String -> IxedString
mkIxedString str =
    IxedString
    { string: toCharArray str
    , ix: 0
    , len: length str
    }

toString :: IxedString -> String
toString (IxedString str) =
    drop str.ix (fromCharArray str.string)

emptyIxedString :: IxedString
emptyIxedString = mkIxedString ""

isEmpty :: IxedString -> Boolean
isEmpty (IxedString str) =
    null str.string || str.ix == str.len

unconsIxed :: IxedString -> Maybe { head :: Char, tail :: IxedString }
unconsIxed (IxedString s) = do
    c <- s.string !! s.ix
    let s' = IxedString
            { string: s.string
            , ix: s.ix + 1
            , len: s.len
            }
    Just $ { head: c, tail: s' }

maybeToEither :: forall a b. b -> Maybe a -> Either b a
maybeToEither b Nothing = Left b
maybeToEither _ (Just x) = Right x
