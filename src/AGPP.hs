-- Author: Paul Reiners
-- https://github.com/paul-reiners/a-golden-prime-project

module AGPP where 

import TUOLP
import Text.Printf
import Control.Exception
import System.CPUTime
import System.Environment

getPrimePairs :: Integer -> [(Integer, Integer)]
getPrimePairs n = [(p, q) | p <- takeWhile (< n) primes, q <- takeWhile (< n) primes, p <= q, p + q == n]

-- See http://www.haskell.org/haskellwiki/Timing_computations
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v
 
main = do
    args <- getArgs
    putStrLn "Starting..."
    time $ getPrimePairs (read $ head args :: Integer) `seq` return ()
    putStrLn "Done."
	