-- Combinators based on John Hughes' paper "The Design of a Pretty-printing Library"
-- TODO: Give a better implementation

module Stratify.Pretty.Doc where

import Prelude

import Data.List

newtype Doc = Doc String

instance Semigroup Doc where
    append = beside

instance Monoid Doc where
    mempty = Doc mempty

atop :: Doc -> Doc -> Doc
atop (Doc x) (Doc y) = Doc (x <> "\n" <> y)

infixr 5 atop as $$

beside :: Doc -> Doc -> Doc
beside (Doc x) (Doc y) = Doc (x <> y)

besideSpace :: Doc -> Doc -> Doc
besideSpace x y = x <> text " " <> y

infixr 5 besideSpace as <+>

parens :: Doc -> Doc
parens x = text "(" <> x <> text ")"

maybeParens :: Boolean -> Doc -> Doc
maybeParens false x = x
maybeParens true x = parens x

sep :: List Doc -> Doc
sep = foldr beside mempty

text :: String -> Doc
text = Doc

render :: Doc -> String
render (Doc s) = s
