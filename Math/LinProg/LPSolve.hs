{-# LANGUAGE ViewPatterns #-}
{-|
Module      : Math.LinProg.LPSolve
Description : Binding for solving LPs with lp_solve library.
Copyright   : (c) Justin Bedő, 2014
License     : BSD
Maintainer  : cu@cua0.org
Stability   : experimental

This module allows finding the solution to an LP using the lp_solve library.
The LP is specified using the monad and expressions in Math.LinProg.Types.
Note that the objective is minimised by default, so negation is needed to
maximise instead.
-}
module Math.LinProg.LPSolve (
  solve
  ,solveWithTimeout
  ,ResultCode(..)
) where

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad
import Data.Hashable
import Data.List hiding (nub)
import Math.LinProg.LP
import Math.LinProg.LPSolve.FFI hiding (solve)
import Math.LinProg.Types
import Prelude hiding (EQ, nub)
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import qualified Math.LinProg.LPSolve.FFI as F

solve :: (Hashable v, Eq v, Ord v) => LinProg Double v () -> IO (Maybe ResultCode, [(v, Double)])
solve = solveWithTimeout 0

-- | Solves an LP using lp_solve.
solveWithTimeout :: (Hashable v, Eq v, Ord v) => Integer -> LinProg Double v () -> IO (Maybe ResultCode, [(v, Double)])
solveWithTimeout t (compile -> lp) = do
    model <- makeLP nconstr nvars
    case model of
      Nothing -> return (Nothing, [])
      Just m' -> with m' $ \m -> do
        setTimeout m t

        -- Eqs
        forM_ (zip [1..] $ lp ^. equals) $ \(i, eq) -> do
          let c = negate $ snd eq
          setConstrType m i EQ
          setRHS m i c
          setRow' m i (fst eq)
          return ()

        -- Leqs
        forM_ (zip [1+nequals..] $ lp ^. leqs) $ \(i, eq) -> do
          let c = negate $ snd eq
          setConstrType m i LE
          setRHS m i c
          setRow' m i (fst eq)
          return ()

        -- Ints
        forM_ (lp ^. ints) $ \v ->
          setInt m (varLUT M.! v)

        -- Bins
        forM_ (lp ^. bins) $ \v ->
          setBin m (varLUT M.! v)

        -- Objective
        setRow' m 0 (lp ^. objective)

        res <- F.solve m
        sol <- snd <$> getSol nvars m
        let vars = zip varList sol
        return (Just res, vars)
  where
    nconstr = length allConstr
    nvars = M.size varLUT
    nequals = length (lp ^. equals)

    allConstr = (lp ^. equals) ++ (lp ^. leqs)
    varList = nub $ concatMap (vars . fst) allConstr ++ vars (lp ^. objective)
    varLUT = M.fromList $ zip varList [1..]

    with m f = do
      r <- f m
      freeLP m
      return r

    nub = S.toList . S.fromList

    setRow' m i eq = setRow m i (map (first ((M.!) varLUT)) $ varTerms eq)
