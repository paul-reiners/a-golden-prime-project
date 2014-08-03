A Golden Prime Project
======================

Goldbach's Conjecture
---------------------
[Goldbach's conjecture](http://en.wikipedia.org/wiki/Goldbach's_conjecture) states that every even integer *n* greater than 2 is the sum of two primes.  It has not been proved, but has been confirmed by computer for *n* up to 4 × 10^18.


This project contains computational experiments regarding Goldbach's conjecture.

Prime Generation
----------------
Rather than write the prime generation code in Haskell myself, I decided to concentrate on code related specifically to Goldbach's conjecture in this project.  Hence I've used Kees Doets' and Jan van Eijck's Sieve of Erastothenes Haskell implementation from [*The Haskell Road to Logic, Maths and Programming*](http://homepages.cwi.nl/~jve/HR/#Home).  This code is in [TUOLP.hs](https://github.com/paul-reiners/a-golden-prime-project/blob/master/src/TUOLP.hs).

(I've written my own prime generation code in C in my [a-prime-project](https://github.com/paul-reiners/a-prime-project/blob/master/prime.c).)

It should be noted that the ["time complexity of calculating all primes below *n* in the random access machine model is *O(n log log n)* operations"](http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#Algorithm_complexity).


Finding Pairs of Primes
-----------------------
I first find all pairs of primes that sum to *n* for a given even number *n > 2*.

My first implementation does the obvious thing:

    primePairs' :: Integer -> [(Integer, Integer)]
    primePairs' n = [(p, q) | p <- takeWhile (< n) primes, q <- takeWhile (< n) primes, p <= q, p + q == n]
    
This clearly runs in quadratic time if you ignore the calls to *primes*.  Here is the performance data:

          n	     primePairs' computation time (sec)
        256                             	  0.008
        512	                                  0.024
      1,024	                                  0.069
      2,048	                                  0.226
      4,096	                                  0.058
      8,192	                                  0.262
     16,384	                                  1.123
     32,768	                                  5.335
     65.536	                                 44.821
    131,072	                                245.860
    262,144	                               1211.950
    524,288	                               4887.736

(Note that my [performance measuring code](https://github.com/paul-reiners/a-golden-prime-project/blob/master/src/TimePairGeneration.hs) isn't the most scientific in the world, which is why the computation time for 2048 is less than the computation time for 4096.  But, whatever.) 

There is an obvious linear time implementation:

    primePairsHelper :: Integer -> [Integer] -> [(Integer, Integer)]
    primePairsHelper n ps 
        | length ps == 0                           = []
        | length ps == 1 && head ps + head ps == n = [(head ps, head ps)]
        | head ps + last ps < n                    = primePairsHelper n (tail ps)
        | head ps + last ps > n                    = primePairsHelper n (init ps)
        | otherwise                                = (head ps, last ps) : primePairsHelper n (tail (init ps))
        
    primePairs :: Integer -> [(Integer, Integer)]
    primePairs n = primePairsHelper n (takeWhile (< n) primes)

However, the speed-up is disappointing:

    n	       primePairs' computation time (sec)	 primePairs computation time (sec)
          256	    0.008	                              0.001
          512	    0.024	                              0.001
        1,024	    0.069	                              0.004
        2,048	    0.226	                              0.018
        4,096	    0.058	                              0.080
        8,192	    0.262	                              0.247
       16,384	    1.123	                              1.161
       32,768	    5.335	                              5.175
       65,536	   44.821	                             43.395
      131,072	  245.860	                            242.805
      262,144	1,211.950	                          1,168.571
      524,288	4,887.736	                          4,806.134
    1,048,576		                                 22,614.785
