module AGPP_Test where

import AGPP( primePairs, primePairCount )
import Test.HUnit

testPrimePairs26 = TestCase $ assertEqual
	"primePairs failed for n = 26" [(3,23),(7,19),(13,13)] ( primePairs 26 )

testPrimePairs100 = TestCase $ assertEqual
	"primePairs failed for n = 100" [(3,97),(11,89),(17,83),(29,71),(41,59),(47,53)] ( primePairs 100 )

testPrimePairCount26 = TestCase $ assertEqual
	"primePairCount failed for n = 26" 3 ( primePairCount 26 )
	
main = runTestTT $ TestList [testPrimePairs26, testPrimePairs100, testPrimePairCount26]
