module TimeMeasure where

import Text.Printf
import Control.Exception
import System.CPUTime

time :: a -> IO a
time computation = do
    start <- getCPUTime
    v <- evaluate computation
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v