{-# LANGUAGE ViewPatterns #-}

module Math.LinProg.LPSolve where

import Control.Applicative
import Control.Monad
import Data.List
import Control.Lens
import Math.LinProg.LPSolve.FFI hiding (solve)
import qualified Math.LinProg.LPSolve.FFI as F
import Math.LinProg.Compile
import Math.LinProg.Types
import qualified Data.Map as M
import Prelude hiding (EQ)

solve :: (Eq v, Ord v) => LinProg Double v () -> IO (Either (Maybe ResultCode) [(v, Double)])
solve (compile -> lp) = do
    model <- makeLP nconstr nvars
    case model of
      Nothing -> return (Left Nothing)
      Just m' -> with m' $ \m -> do

        -- Eqs
        forM_ (zip [1..] $ lp ^. equals) $ \(i, eq) ->
          forM_ (M.keys varLUT) $ \v -> do
            let w = getVar v $ fst eq
                c = negate $ snd eq
            when (w /= 0) $ do
              setMat m i (varLUT M.! v) w
              setConstrType m i EQ
              setRHS m i c
              return ()

        -- Leqs
        forM_ (zip [1+nequals..] $ lp ^. leqs) $ \(i, eq) ->
          forM_ (M.keys varLUT) $ \v -> do
            let w = getVar v $ fst eq
                c = negate $ snd eq
            when (w /= 0) $ do
              setMat m i (varLUT M.! v) w
              setConstrType m i LE
              setRHS m i c
              return ()

        -- Objective
        forM_ (M.keys varLUT) $ \v -> do
          let w = getVar v $ lp ^. objective
          when (w /= 0) $ void $ setMat m 0 (varLUT M.! v) w

        res <- F.solve m
        case res of
          Optimal -> do
            sol <- snd <$> getSol nvars m
            return $ Right (zip (M.keys varLUT) sol)
          _ -> return $ Left (Just res)
  where
    nconstr = length allConstr
    nvars = M.size varLUT
    nequals = length (lp ^. equals)

    allConstr = (lp ^. equals) ++ (lp ^. leqs)
    varLUT = M.fromList $ zip (sort $ nub $ concatMap (vars . fst) allConstr) [1..]

    with m f = do
      r <- f m
      freeLP m
      return r
