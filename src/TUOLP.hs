-- From "The Haskell Road to Logic, Maths and Programming" by Kees Doets and Jan van Eijck
-- http://homepages.cwi.nl/~jve/HR/#Home

module TUOLP 

where 

evens = [ x | x <- [0..], even x ]

prime :: Integer -> Bool 
prime n | n < 1     = error "not a positive integer"
        | n == 1    = False 
        | otherwise = ldp n == n where 
  ldp    = ldpf primes
  ldpf (p:ps) m | rem m p == 0 = p
                | p^2 > m      = m
                | otherwise    = ldpf ps m
  primes = 2 : filter prime [3..]

sieve :: [Integer] -> [Integer]
sieve (0 : xs) = sieve xs
sieve (n : xs) = n : sieve (mark xs 1 n)
  where 
  mark :: [Integer] -> Integer -> Integer -> [Integer]
  mark (y:ys) k m | k == m    =  0 : (mark ys  1    m)
                  | otherwise =  y : (mark ys (k+1) m)

primes :: [Integer]
primes = sieve [2..]
