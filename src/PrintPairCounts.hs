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

primeDivisorCounts :: Integer -> [Int]
primeDivisorCounts n = map primeDivisorCount (nums n)

numsAndPrimePairCounts :: Integer -> [(Integer, Int, Int)]
numsAndPrimePairCounts n = zip3 (nums n) (primePairCounts n) (primeDivisorCounts n)

showTup :: (Show a, Show b, Show c) => (a,b,c) -> String
showTup (a,b,c) = (show a) ++ "," ++ (show b) ++ "," ++ (show c)

main = do
    args <- getArgs
    mapM_ (putStrLn . showTup) (numsAndPrimePairCounts (read $ head args :: Integer))
