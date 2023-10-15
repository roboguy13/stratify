module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Stratify.Syntax.Core.Term
import Stratify.Eval.NbE
import Stratify.Syntax.Name

import Data.List
import Bound

main :: Effect Unit
main = do
  log $ show $ eval emptyNameEnv (fromNamed (App (Lam "x" IntType (Var "x")) (IntLit 1)))
  log $ show $ eval emptyNameEnv $ fromNamed $
    App (App (Lam "x" (fnType IntType IntType)
                (Lam "y" IntType (Op (Sub (Var "x") (Var "y")))))
             (IntLit 10))
        (IntLit 1)
