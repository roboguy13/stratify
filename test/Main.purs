module Test.Main
  ( main
  )
  where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

import Stratify.Syntax.Core.Term
import Stratify.Eval.NbE
import Stratify.Syntax.Name
import Stratify.Ppr
import Stratify.Utils

import Data.List
import Data.Either

import Bound

main :: Effect Unit
main = do
  log $ ppr' $ eval emptyNameEnv (fromNamed (App (Lam "x" IntType (Var "x")) (IntLit 1)))
  log $ ppr' $ eval emptyNameEnv $ fromNamed $
    App (App (Lam "x" (fnType IntType IntType)
                (Lam "y" IntType (Op (Sub (Var "x") (Var "y")))))
             (IntLit 10))
        (IntLit 1)
  log $ ppr' $ eval emptyNameEnv $ fromNamed $
    Lam "x" IntType (Var "x")
  log $ ppr' $ eval emptyNameEnv $ fromNamed $
    Lam "x" IntType (Op (Add (Var "x") (Op (Add (IntLit 1) (IntLit 2)))))
  log $ ppr' $ eval emptyNameEnv $ fromNamed $
    Lam "x" IntType
      $ Lam "y" IntType
          (Op (Sub (Var "x") (Op (Add (Var "y") (IntLit 2)))))

ppr' (Right x) = ppr x
ppr' (Left y) = error y
