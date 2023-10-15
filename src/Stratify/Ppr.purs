module Stratify.Ppr where

import Prelude

import Stratify.Pretty.Doc

class Ppr a where
    pprDoc :: a -> Doc

instance Ppr String where pprDoc = text

ppr :: forall a. Ppr a => a -> String
ppr = render <<< pprDoc

pprNested :: forall a. Nested a => a -> Doc
pprNested x = maybeParens (isNested x) (pprDoc x)

-- parens :: Doc -> Doc
-- parens x = text "(" <> x <> text ")"

-- TODO: Precedence class
class Ppr a <= Nested a where
    isNested :: a -> Boolean
