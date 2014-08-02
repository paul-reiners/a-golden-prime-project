-- Author: Paul Reiners
-- https://github.com/paul-reiners/a-golden-prime-project

module AGPP where 

import TUOLP

primePairs' :: Integer -> [(Integer, Integer)]
primePairs' n = [(p, q) | p <- takeWhile (< n) primes, q <- takeWhile (< n) primes, p <= q, p + q == n]

primePairsHelper :: Integer -> [Integer] -> [(Integer, Integer)]
primePairsHelper n ps 
    | length ps == 0                           = []
    | length ps == 1 && head ps + head ps == n = [(head ps, head ps)]
    | head ps + last ps < n                    = primePairsHelper n (tail ps)
    | head ps + last ps > n                    = primePairsHelper n (init ps)
    | otherwise                                = (head ps, last ps) : primePairsHelper n (tail (init ps))
    
primePairs :: Integer -> [(Integer, Integer)]
primePairs n = primePairsHelper n (takeWhile (< n) primes)
