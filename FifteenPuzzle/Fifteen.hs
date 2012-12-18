module Main where

import Text.Printf
import Control.Exception
import System.CPUTime

import FifteenPuzzle

main = do
    start <- getCPUTime
    
    print start4''''
    let solution = doSearch goal4 start4''''
    print solution
    
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    
    
