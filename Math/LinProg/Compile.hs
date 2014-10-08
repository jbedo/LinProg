{-# LANGUAGE TemplateHaskell, FlexibleInstances, ScopedTypeVariables #-}

module Math.LinProg.Compile (
  compile
  ,Equation
  ,CompilerS(..)
  ,objective
  ,equals
  ,leqs
) where

import Math.LinProg.Types
import Control.Lens
import Control.Monad.Free

type Equation t v = (LinExpr t v, t) -- LHS and RHS

data CompilerS t v = CompilerS {
  _objective :: LinExpr t v
  ,_equals :: [Equation t v]
  ,_leqs :: [Equation t v]
} deriving (Eq)

$(makeLenses ''CompilerS)

compile :: (Num t, Show t, Ord t, Eq v) => LinProg t v () -> CompilerS t v
compile ast = compile' ast initCompilerS where
  compile' (Free (Objective a c)) state = compile' c $ state & objective +~ a
  compile' (Free (EqConstraint a b c)) state = compile' c $ state & equals %~ (split (a-b):)
  compile' (Free (LeqConstraint a b c)) state = compile' c $ state & leqs %~ (split (a-b):)
  compile' _ state = state

  initCompilerS = CompilerS
    0
    []
    []

