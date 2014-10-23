{-# LANGUAGE TemplateHaskell, FlexibleInstances, ScopedTypeVariables #-}

module Math.LinProg.LP (
  compile
  ,Equation
  ,CompilerS(..)
  ,objective
  ,equals
  ,leqs
) where

import Data.List
import Math.LinProg.Types
import Control.Lens
import Data.Maybe
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

-- Printing to LP format
instance (Show t, Num t, Ord t) => Show (CompilerS t String) where
  show s = unlines $ catMaybes [
      Just "Minimize"
      ,Just (showEq $ varTerms (s ^. objective))
      ,if hasST then Just "Subject to" else Nothing
      ,if hasEqs then Just (intercalate "\n" $ map (\(a, b) -> showEq a ++ " = " ++ show (negate b)) $ s ^. equals) else Nothing
      ,if hasUnbounded then Just (intercalate "\n" $ map (\(a, b) -> showEq a ++ " <= " ++ show (negate b)) unbounded) else Nothing
      ,if hasBounded then Just "Bounds" else Nothing
      ,if hasBounded then Just (intercalate "\n" $ map (\(l, v, u) -> show l ++ " <= " ++ v ++ " <= " ++ show u) bounded) else Nothing
    ]
    where
      getVars eq = zip vs ws
        where
          vs = vars eq
          ws = map (`getVar` eq) vs

      showEq = unwords . map (\(a, b) -> render b ++ " " ++ a) . getVars

      (bounded, unbounded) = findBounds $ s ^. leqs
      hasBounded = not (null bounded)
      hasUnbounded = not (null unbounded)
      hasEqs = not (null (s^.equals))
      hasST = hasUnbounded || hasEqs

      render x = (if x >= 0 then "+" else "") ++ show x

findBounds :: (Eq v, Num t, Ord t, Eq t) => [Equation t v] -> ([(t, v, t)], [Equation t v])
findBounds eqs = (mapMaybe bound singleTerms, eqs \\ filter (isBounded . head . vars . fst) singleTermEqs)
  where
    singleTermEqs = filter (\(ts, _) -> length (vars ts) == 1) eqs
    singleTerms = nub $ concatMap (vars . fst) singleTermEqs

    upperBound x = mapMaybe (\(a, c) -> let w = getVar x a in if w == 1 then Just (negate c) else Nothing) singleTermEqs
    lowerBound x = mapMaybe (\(a, c) -> let w = getVar x a in if w == -1 then Just c else Nothing) singleTermEqs

    bound v = bound' (lowerBound v) (upperBound v) where
      bound' [] _ = Nothing
      bound' _ [] = Nothing
      bound' ls us | l <= u = Just (l, v, u)
                   | otherwise = Nothing where
        l = maximum ls
        u = minimum us

    isBounded v = isJust (bound v)
