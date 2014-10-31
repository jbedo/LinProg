import Criterion.Main
import Math.LinProg.LP
import Math.LinProg.LPSolve
import Math.LinProg.Types

benchLP :: Int -> IO (Maybe ResultCode, [(Int, Double)])
benchLP n =
  solve $ obj $ sum $ map var [1..n]

benchEq :: Int -> [(Int, Double)]
benchEq n = varTerms $ sum $ map var [1..n]

benchVars :: Int -> [Int]
benchVars n = vars $ sum $ map var [1..n]

benchCompile :: Int -> CompilerS Double String
benchCompile n = compile $ obj $ sum $ map (var . show) [1..n]

benchShow :: Int -> IO ()
benchShow n = print (benchCompile n)

main = defaultMain [
  bgroup "compile" [ bench "100" $ whnf benchCompile 100
                   , bench "1000" $ whnf benchCompile 1000
                   , bench "10000" $ whnf benchCompile 10000 ]
  --,bgroup "compile-show" [ bench "100" $ whnfIO (benchShow 100)
  --                      , bench "1000" $ whnfIO (benchShow 1000)
  --                      , bench "10000" $ whnfIO (benchShow 10000) ]
  ,bgroup "vars" [ bench "100" $ whnf benchVars 100
                 , bench "1000" $ whnf benchVars 1000
                 , bench "10000" $ whnf benchVars 10000 ]
  ,bgroup "eq" [ bench "100" $ whnf benchEq 100
               , bench "1000" $ whnf benchEq 1000
               , bench "10000" $ whnf benchEq 10000 ]
  ,bgroup "LP" [ bench "100" $ whnfIO (benchLP 100)
               , bench "1000" $ whnfIO (benchLP 1000)
               , bench "10000" $ whnfIO (benchLP 10000)]
  ]
