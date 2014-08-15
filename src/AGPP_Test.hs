-- Author: Paul Reiners
-- https://github.com/paul-reiners/a-golden-prime-project

module AGPP_Test where

import AGPP( primePairs, primePairCount, weakGoldbachTriples )
import Test.HUnit

testPrimePairs26 = TestCase $ assertEqual
	"primePairs failed for n = 26" [(3,23),(7,19),(13,13)] ( primePairs 26 )

testPrimePairs100 = TestCase $ assertEqual
	"primePairs failed for n = 100" [(3,97),(11,89),(17,83),(29,71),(41,59),(47,53)] ( primePairs 100 )

testPrimePairCount26 = TestCase $ assertEqual
	"primePairCount failed for n = 26" 3 ( primePairCount 26 )

testPrimePairCount100 = TestCase $ assertEqual
	"primePairCount failed for n = 100" 6 ( primePairCount 100 )

testWeakGoldbachTriples = TestCase $ assertEqual
	"weakGoldbachTriples failed" [(3,5,13),(3,7,11),(5,5,11),(7,7,7)] ( weakGoldbachTriples 21 )
	
main = runTestTT $ 
	TestList [
		testPrimePairs26, testPrimePairs100, testPrimePairCount26, 
		testPrimePairCount100, testWeakGoldbachTriples]
