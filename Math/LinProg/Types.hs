{-# LANGUAGE DeriveFunctor, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Math.LinProg.Types (
  LinExpr
  ,var
  ,vars
  ,varTerms
  ,getVar
  ,split
  ,LinProg
  ,LinProg'(..)
  ,obj
  ,(<:)
  ,(=:)
  ,(>:)
  ,eq
  ,leq
  ,geq
) where

import Data.Functor.Foldable
import Control.Monad.Free
import qualified Data.Map as M

data LinExpr' t v a =
  Lit t
  | Var v
  | Add a a
  | Mul a a
  | Negate a
  deriving (Show, Eq, Functor)

type LinExpr t v = Fix (LinExpr' t v)

var = Fix . Var

instance Num t => Num (LinExpr t v) where
  a * b = Fix (Mul a b)
  a + b = Fix (Add a b)
  negate a = Fix (Negate a)
  fromInteger a = Fix (Lit (fromInteger a))
  abs = undefined
  signum = undefined

consts :: Num t => LinExpr t v -> t
consts = cata consts' where
  consts' (Negate a) = negate a
  consts' (Lit a) = a
  consts' (Var _) = 0
  consts' (Add a b) = a + b
  consts' (Mul a b) = a * b

getVar :: (Num t, Eq v) => v -> LinExpr t v -> t
getVar id x = cata getVar' x - consts x where
  getVar' (Var x) | x == id = 1
                  | otherwise = 0
  getVar' (Lit a) = a
  getVar' (Add a b) = a + b
  getVar' (Mul a b) = a * b
  getVar' (Negate a) = negate a

vars :: LinExpr t v -> [v]
vars = cata vars' where
  vars' (Var x) = [x]
  vars' (Add a b) = a ++ b
  vars' (Mul a b) = a ++ b
  vars' (Negate a) = a
  vars' _ = []

varTerms eq = go eq' where
  go [t] = t
  go (t:ts) = Fix (Add t (go ts))
  go [] = Fix (Lit 0)

  eq' = zipWith (\v w -> Fix (Mul (Fix (Lit w)) (Fix (Var v)))) vs ws
  vs = vars eq
  ws = map (`getVar` eq) vs

split :: (Num t, Eq v) => LinExpr t v -> (LinExpr t v, t)
split eq = (varTerms eq, consts eq)

prettyPrint :: (Show t, Show v) => LinExpr t v -> String
prettyPrint = cata prettyPrint' where
  prettyPrint' (Lit a) = show a
  prettyPrint' (Mul a b) = concat ["(", a, "×", b, ")"]
  prettyPrint' (Add a b) = concat ["(", a, "+", b, ")"]
  prettyPrint' (Var x) = show x

-- Monad for linear programs

data LinProg' t v a =
  Objective (LinExpr t v) a
  | EqConstraint (LinExpr t v) (LinExpr t v) a
  | LeqConstraint (LinExpr t v) (LinExpr t v) a
  deriving (Show, Eq, Functor)

type LinProg t v = Free (LinProg' t v)

obj a = liftF (Objective a ())
eq a b = liftF (EqConstraint a b ())
leq a b = liftF (LeqConstraint a b ())
geq b a = liftF (LeqConstraint a b ())

a =: b = eq a b
a <: b = leq a b
a >: b = geq a b
