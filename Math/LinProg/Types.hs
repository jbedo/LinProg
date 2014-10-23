{-# LANGUAGE DeriveFunctor, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-|
Module      : Math.LinProg.Types
Description : Base types for equations and optimisation monad
Copyright   : (c) Justin Bedő, 2014
License     : BSD
Maintainer  : cu@cua0.org
Stability   : experimental

This module defines the base types for representing equations and linear
programs.  The linear program is created as a free monad, and equations as an
AST.  Note that expressions are assumed to be linear expressions and hence
there is no explicit checking for higher order terms.
-}
module Math.LinProg.Types (
  LinExpr
  ,LinExpr'(..)
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
) where

import Data.Functor.Foldable
import Control.Monad.Free

-- | Base AST for expressions.  Expressions have factors or type t and
-- variables referenced by ids of type v.
data LinExpr' t v a =
  Lit t
  | Var v
  | Add a a
  | Mul a a
  | Negate a
  deriving (Show, Eq, Functor)

type LinExpr t v = Fix (LinExpr' t v)

-- | Creates a new variable for reference in equations
var = Fix . Var

-- | For convient notation, expressions are declared as instances of num.
-- However, linear expressions cannot implement absolute value or sign
-- functions, hence these two remain undefined.
instance Num t => Num (LinExpr t v) where
  a * b = Fix (Mul a b)
  a + b = Fix (Add a b)
  negate a = Fix (Negate a)
  fromInteger a = Fix (Lit (fromInteger a))
  abs = undefined
  signum = undefined

-- | Linear expressions can also be instances of fractional.
instance Fractional t => Fractional (LinExpr t v) where
  a / b = Fix (Mul a (1/b))
  fromRational a = Fix (Lit (fromRational a))

-- | Reduce a linear expression down to the constant factor.
consts :: Num t => LinExpr t v -> t
consts = cata consts' where
  consts' (Negate a) = negate a
  consts' (Lit a) = a
  consts' (Var _) = 0
  consts' (Add a b) = a + b
  consts' (Mul a b) = a * b

-- | Gets the multiplier for a particular variable.
getVar :: (Num t, Eq v) => v -> LinExpr t v -> t
getVar id x = cata getVar' x - consts x where
  getVar' (Var x) | x == id = 1
                  | otherwise = 0
  getVar' (Lit a) = a
  getVar' (Add a b) = a + b
  getVar' (Mul a b) = a * b
  getVar' (Negate a) = negate a

-- | Gets all variables used in an equation.
vars :: LinExpr t v -> [v]
vars = cata vars' where
  vars' (Var x) = [x]
  vars' (Add a b) = a ++ b
  vars' (Mul a b) = a ++ b
  vars' (Negate a) = a
  vars' _ = []

-- | Reduces an expression to the variable terms
varTerms eq = go eq' where
  go [t] = t
  go (t:ts) = Fix (Add t (go ts))
  go [] = Fix (Lit 0)

  eq' = zipWith (\v w -> Fix (Mul (Fix (Lit w)) (Fix (Var v)))) vs ws
  vs = vars eq
  ws = map (`getVar` eq) vs

-- | Splits an expression into the variables and the constant term
split :: (Num t, Eq v) => LinExpr t v -> (LinExpr t v, t)
split eq = (varTerms eq, consts eq)

prettyPrint :: (Show t, Show v) => LinExpr t v -> String
prettyPrint = cata prettyPrint' where
  prettyPrint' (Lit a) = show a
  prettyPrint' (Mul a b) = concat ["(", a, "×", b, ")"]
  prettyPrint' (Add a b) = concat ["(", a, "+", b, ")"]
  prettyPrint' (Var x) = show x

-- | Free monad for linear programs.  The monad allows definition of the
-- objective function, equality constraints, and inequality constraints (≤ only
-- in the data type).
data LinProg' t v a =
  Objective (LinExpr t v) a
  | EqConstraint (LinExpr t v) (LinExpr t v) a
  | LeqConstraint (LinExpr t v) (LinExpr t v) a
  deriving (Show, Eq, Functor)

type LinProg t v = Free (LinProg' t v)

-- | Define a term in the objective function
obj a = liftF (Objective a ())

-- | Define an equality constraint
a =: b = liftF (EqConstraint a b ())

-- | Define an inequality (less than equal) contraint
a <: b = liftF (LeqConstraint a b ())
--
-- | Define an inequality (greater than equal) contraint
b >: a = liftF (LeqConstraint a b ())

infix 4 =:
infix 4 <:
infix 4 >:
