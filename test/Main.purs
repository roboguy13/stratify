module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Stratify.Syntax.Core.Term
import Stratify.Eval.NbE

import Data.List
import Bound

main :: Effect Unit
main = do
  log $ show $ eval Nil (App (Lam "x" IntType (abstract1 "x" (Var "x"))) (IntLit 1))
  log $ show $ eval Nil $
    App (App (lam "x" (fnType IntType IntType)
                (lam "y" IntType (Op (Sub (Var "x") (Var "y")))))
             (IntLit 10))
        (IntLit 1)
