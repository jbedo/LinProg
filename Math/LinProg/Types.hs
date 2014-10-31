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
  ,bin
  ,int
) where

import Control.Applicative
import Control.Monad.Free
import Data.Functor.Foldable
import Data.Hashable
import Data.List
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Test.QuickCheck

-- | Base AST for expressions.  Expressions have factors or type t and
-- variables referenced by ids of type v.
data LinExpr' t v a =
  Lit !t
  | Var !v
  | Wvar !t !v
  | Add !a !a
  | Mul !a !a
  | Negate !a
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
  consts' (Add a b) = a + b
  consts' (Mul a b) = a * b
  consts' _ = 0

-- | Gets the multiplier for a particular variable.
getVar :: (Num t, Eq v) => v -> LinExpr t v -> t
getVar id x = cata getVar' x - consts x where
  getVar' (Wvar w x) | x == id = w
                     | otherwise = 0
  getVar' (Var x) | x == id = 1
                  | otherwise = 0
  getVar' (Lit a) = a
  getVar' (Add a b) = a + b
  getVar' (Mul a b) = a * b
  getVar' (Negate a) = negate a

-- | Gets all variables used in an equation.
vars :: (Hashable v, Eq v) => LinExpr t v -> [v]
vars = S.toList . cata vars' where
  vars' (Wvar _ x) = S.fromList [x]
  vars' (Var x) = S.fromList [x]
  vars' (Add a b) = S.union a b
  vars' (Mul a b) = S.union a b
  vars' (Negate a) = a
  vars' _ = S.empty

-- | Expands terms to Wvars but does not collect like terms
rewrite :: (Eq t, Num t) => LinExpr t v -> LinExpr t v
rewrite = cata rewrite' where
  rewrite' (Var a) = Fix (Wvar 1 a)
  rewrite' (Add (Fix (Lit _)) a@(Fix (Wvar _ _))) = a
  rewrite' (Add a@(Fix (Wvar _ _)) (Fix (Lit _))) = a
  rewrite' (Mul (Fix (Lit a)) (Fix (Wvar b c))) = Fix (Wvar (a * b) c)
  rewrite' (Mul (Fix (Wvar b c)) (Fix (Lit a))) = Fix (Wvar (a * b) c)
  rewrite' (Add (Fix (Lit a)) (Fix (Lit b))) = Fix (Lit (a + b))
  rewrite' (Mul (Fix (Lit a)) (Fix (Lit b))) = Fix (Lit (a * b))
  rewrite' (Lit a) = Fix (Lit a)
  rewrite' (Mul (Fix (Add a b)) c) = rewrite' (Add (rewrite' (Mul a c)) (rewrite' (Mul b c)))
  rewrite' (Mul c (Fix (Add a b))) = rewrite' (Add (rewrite' (Mul a c)) (rewrite' (Mul b c)))
  rewrite' (Negate (Fix (Wvar a b))) = Fix (Wvar (negate a) b)
  rewrite' (Negate (Fix (Lit a))) = Fix (Lit (negate a))
  rewrite' (Negate (Fix (Add a b))) = rewrite' (Add (rewrite' (Negate a)) (rewrite' (Negate b)))
  rewrite' (Negate (Fix (Mul a b))) = rewrite' (Add (rewrite' (Negate a)) b)
  rewrite' a = Fix a

-- | Reduces an expression to the variable terms
varTerms :: (Num t, Eq t, Hashable v, Eq v) => LinExpr t v -> [(v, t)]
varTerms = M.toList . cata go . rewrite where
  go (Wvar w a) = M.fromList [(a, w)]
  go (Add a b) = M.unionWith (+) a b
  go (Mul _ _) = error "Only linear terms supported"
  go _ = M.empty

-- | Splits an expression into the variables and the constant term
split :: (Num t, Eq v) => LinExpr t v -> (LinExpr t v, t)
split eq = (eq - (Fix (Lit (consts eq))), consts eq)

prettyPrint :: (Show t, Show v) => LinExpr t v -> String
prettyPrint = cata prettyPrint' where
  prettyPrint' (Lit a) = show a
  prettyPrint' (Mul a b) = concat ["(", a, "×", b, ")"]
  prettyPrint' (Add a b) = concat ["(", a, "+", b, ")"]
  prettyPrint' (Var x) = show x
  prettyPrint' (Wvar w x) = show w ++ show x

-- | Free monad for linear programs.  The monad allows definition of the
-- objective function, equality constraints, and inequality constraints (≤ only
-- in the data type).
data LinProg' t v a =
  Objective !(LinExpr t v) !a
  | Integer !v !a
  | Binary !v !a
  | EqConstraint !(LinExpr t v) !(LinExpr t v) !a
  | LeqConstraint !(LinExpr t v) !(LinExpr t v) !a
  deriving (Show, Eq, Functor)

type LinProg t v = Free (LinProg' t v)

-- | Define a term in the objective function
obj a = liftF (Objective a ())

-- | Define an equality constraint
a =: b = liftF (EqConstraint a b ())

-- | Define an inequality (less than equal) contraint
a <: b = liftF (LeqConstraint a b ())

-- | Define an inequality (greater than equal) contraint
b >: a = liftF (LeqConstraint a b ())

-- | Declare a variable to be binary
bin (Fix (Var v)) = liftF (Binary v ())

-- | Declare a variable to be integral
int (Fix (Var v)) = liftF (Integer v ())

infix 4 =:
infix 4 <:
infix 4 >:

-- Quickcheck properties

instance (Arbitrary t, Arbitrary v) => Arbitrary (LinExpr t v) where
  arbitrary = oneof [
    (Fix . Var) <$> arbitrary
    ,(Fix . Lit) <$> arbitrary
    ,((Fix .) . Add) <$> arbitrary <*> arbitrary
    ,((Fix .) . Mul) <$> arbitrary <*> arbitrary
    ,(Fix . Negate) <$> arbitrary]

prop_rewrite :: LinExpr Int Int -> Property
prop_rewrite eq = isLinear eq ==> sort (zip vs ws) == sort (varTerms eq)
  where
    vs = vars eq
    ws = map (flip getVar eq) vs
    isLinear = (<= 1) . cata isLinear' where
      isLinear' (Mul a b) = a + b
      isLinear' (Add a b) = max a b
      isLinear' (Var _) = 1
      isLinear' (Wvar _ _) = 1
      isLinear' (Lit _) = 0
      isLinear' (Negate a) = a
