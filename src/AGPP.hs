-- Author: Paul Reiners
-- https://github.com/paul-reiners/a-golden-prime-project

module AGPP where 

import TUOLP

getPrimePairs' :: Integer -> [(Integer, Integer)]
getPrimePairs' n = [(p, q) | p <- takeWhile (< n) primes, q <- takeWhile (< n) primes, p <= q, p + q == n]

getPrimePairsHelper :: Integer -> [Integer] -> [(Integer, Integer)]
getPrimePairsHelper n ps 
    | length ps == 0                           = []
    | length ps == 1 && head ps + head ps == n = [(head ps, head ps)]
    | head ps + last ps < n                    = getPrimePairsHelper n (tail ps)
    | head ps + last ps > n                    = getPrimePairsHelper n (init ps)
    | otherwise                                = (head ps, last ps) : getPrimePairsHelper n (tail (init ps))
    
getPrimePairs :: Integer -> [(Integer, Integer)]
getPrimePairs n = getPrimePairsHelper n (takeWhile (< n) primes)
