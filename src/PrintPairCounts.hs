-- Author: Paul Reiners
-- https://github.com/paul-reiners/a-golden-prime-project

module PrintPairCounts where 

import AGPP
import Text.Printf
import Control.Exception
import System.CPUTime
import System.Environment

nums :: Integer -> [Integer]
nums n = [m | m <- [4..n], even m]

primePairCounts :: Integer -> [Int]
primePairCounts n = map primePairCount (nums n)

numsAndPrimePairCounts :: Integer -> [(Integer, Int)]
numsAndPrimePairCounts n = zip (nums n) (primePairCounts n)

main = do
    args <- getArgs
    mapM_ print (numsAndPrimePairCounts (read $ head args :: Integer))
