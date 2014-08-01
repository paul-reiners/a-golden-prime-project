-- Author: Paul Reiners
-- https://github.com/paul-reiners/a-golden-prime-project

module AGPP where 

import TUOLP

getPrimePairs :: Integer -> [(Integer, Integer)]
getPrimePairs n = [(p, q) | p <- takeWhile (< n) primes, q <- takeWhile (< n) primes, p <= q, p + q == n]
