This library implements a simple eDSL for linear programming and a simple
wrapper around lp_solve (potentially other solvers can also be plugged in
easily).  Here's how to solve the farmer example from the lp_solve
documentation:

> Suppose a farmer has 75 acres on which to plant two crops: wheat and barley.
> To produce these crops, it costs the farmer (for seed, fertilizer, etc.) $120
> per acre for the wheat and  $210 per acre for the barley. The farmer has
> $15000 available for expenses. But after the harvest, the farmer must store
> the crops while awaiting favourable market conditions. The farmer has storage
> space for 4000 bushels. Each acre yields an average of 110 bushels of wheat
> or 30 bushels of barley.  If the net profit per bushel of wheat (after all
> expenses have been subtracted) is $1.30 and for barley is $2.00, how should
> the farmer plant the 75 acres to maximize profit?

    import Control.Monad
    import Math.LinProg.LPSolve
    import Math.LinProg.Types

    data Crop = Wheat | Barley
      deriving (Eq, Show, Ord)

    lp :: LinProg Double Crop ()
    lp = do
      let vs@[w, b] = map var [Wheat, Barley]
      obj $ negate $ 110 * 1.3 * w + 30 * 2 * b
      120 * w + 210 * b <: 15000
      110 * w + 30 * b <: 4000
      w + b <: 75

    main :: IO ()
    main = do
      sol <- solve lp
      print sol

This outputs the solution: Right [(Wheat,21.875),(Barley,53.12499999999999)].
Due to the monadic structure one can build up LPs using the usual monadic
controls such as mapM/forM etc, making it quite easy to specify constraints.
