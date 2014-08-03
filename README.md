A Golden Prime Project
======================

Goldbach's Conjecture
---------------------
[Goldbach's conjecture](http://en.wikipedia.org/wiki/Goldbach's_conjecture) states that every even integer *n* greater than 2 is the sum of two primes.  It has not been proved, but has been confirmed by computer for *n* up to 4 × 10^18.


This project contains computational experiments regarding Goldbach's conjecture.

Prime Generation
----------------
Rather than write the prime generation code in Haskell myself, I decided to concentrate on code related to Goldbach's conjecture in this project.  I've used the Kees Doets' and Jan van Eijck's Sieve of Erastothenes Haskell implementation from [*The Haskell Road to Logic, Maths and Programming*](http://homepages.cwi.nl/~jve/HR/#Home) by .

I've written my own prime generation code in C in my [a-prime-project](https://github.com/paul-reiners/a-prime-project/blob/master/prime.c).

It should be noted that the ["time complexity of calculating all primes below *n* in the random access machine model is *O(n log log n)* operations"](http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#Algorithm_complexity).


Finding Pairs of Primes
-----------------------
AGPP.getPrimePairs returns all prime pairs that sum to n for input argument n.  For n up to about 57,000, it returns all prime pairs in less than 30 seconds.
