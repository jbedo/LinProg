{-# LANGUAGE ForeignFunctionInterface #-}
module Math.LinProg.LPSolve.FFI (
  ResultCode(..)
  ,ConstraintType(..)
  ,LPRec
  ,setConstrType
  ,setTimeout
  ,setInt
  ,setBin
  ,makeLP
  ,freeLP
  ,setMat
  ,setRHS
  ,solve
  ,getSol
  ,debugDump
) where

import Foreign
import Foreign.C
import Control.Applicative ((<$>))
import qualified Data.Map as M

type LPRec = Ptr ()

data ResultCode =
  NoMemory
  |Optimal
  |SubOptimal
  |Infeasible
  |Unbounded
  |Degenerate
  |NumFailure
  |UserAbort
  |Timeout
  |PreSolved
  |ProcFail
  |ProcBreak
  |FeasFound
  |NoFeasFound
  deriving (Show, Eq, Ord)

data ConstraintType =
  Fr
  |LE
  |GE
  |EQ
  deriving (Show, Eq, Ord, Enum)

foreign import ccall "make_lp" c_make_lp :: CInt -> CInt -> IO LPRec
foreign import ccall "free_lp" c_free_lp :: Ptr LPRec -> IO ()
foreign import ccall "set_mat" c_set_mat :: LPRec -> CInt -> CInt -> CDouble -> IO CChar
foreign import ccall "set_rh" c_set_rh :: LPRec -> CInt -> CDouble -> IO CChar
foreign import ccall "solve" c_solve :: LPRec -> IO CInt
foreign import ccall "get_variables" c_get_variables :: LPRec -> Ptr CDouble -> IO CChar
foreign import ccall "set_constr_type" c_set_constr_type :: LPRec -> CInt -> CInt -> IO CChar
foreign import ccall "set_timeout" c_set_timeout :: LPRec -> CLong -> IO ()
foreign import ccall "set_int" c_set_int :: LPRec -> CInt -> CChar -> IO CChar
foreign import ccall "set_binary" c_set_binary :: LPRec -> CInt -> CChar -> IO CChar
foreign import ccall "print_debugdump" c_print_debugdump :: LPRec -> CString -> IO ()

debugDump :: LPRec -> FilePath -> IO ()
debugDump lp path = withCString path $ \str -> c_print_debugdump lp str

setTimeout :: LPRec -> Integer -> IO ()
setTimeout lp x = c_set_timeout lp (fromIntegral x)

setConstrType :: LPRec -> Int -> ConstraintType -> IO Word8
setConstrType lp i t = fromIntegral <$> c_set_constr_type lp (fromIntegral i) (fromIntegral $ fromEnum t)

makeLP :: Int -> Int -> IO (Maybe LPRec)
makeLP n m = do
  m' <- c_make_lp (fromIntegral n) (fromIntegral m)
  return $ if m' == nullPtr then
    Nothing
  else
    Just m'

freeLP :: LPRec -> IO ()
freeLP m = with m $ \m' -> c_free_lp m'

setMat :: LPRec -> Int -> Int -> Double -> IO Word8
setMat a b c d = fromIntegral <$> c_set_mat a (fromIntegral b) (fromIntegral c) (realToFrac d)

setRHS :: LPRec -> Int -> Double -> IO Word8
setRHS a b c = fromIntegral <$> c_set_rh a (fromIntegral b) (realToFrac c)

setInt :: LPRec -> Int -> IO Word8
setInt m a = fromIntegral <$> c_set_int m (fromIntegral a) 1

setBin :: LPRec -> Int -> IO Word8
setBin m a = fromIntegral <$> c_set_binary m (fromIntegral a) 1

solve :: LPRec -> IO ResultCode
solve lp = (lut M.!) . fromIntegral <$> c_solve lp
  where
    lut = M.fromList [
        (-2, NoMemory)
        ,(0 :: Int, Optimal)
        ,(1, SubOptimal)
        ,(2, Infeasible)
        ,(3, Unbounded)
        ,(4, Degenerate)
        ,(5, NumFailure)
        ,(6, UserAbort)
        ,(7, Timeout)
        ,(9, PreSolved)
        ,(10, ProcFail)
        ,(11, ProcBreak)
        ,(12, FeasFound)
        ,(13, NoFeasFound)
      ]

getSol :: Int -> LPRec -> IO (Word8, [Double])
getSol n lp = allocaArray (1+n) $ \arr -> do
    res <- c_get_variables lp arr
    vals <- peekArray (1+n) arr
    let vs = map realToFrac vals
    return (fromIntegral res, vs)
