module AGPP_Test where

import AGPP( getPrimePairs )
import Test.HUnit

testGetPrimePairs26 = TestCase $ assertEqual
	"getPrimePairs failed for n = 26" [(3,23),(7,19),(13,13)] ( getPrimePairs 26 )

testGetPrimePairs100 = TestCase $ assertEqual
	"getPrimePairs failed for n = 100" [(3,97),(11,89),(17,83),(29,71),(41,59),(47,53)] ( getPrimePairs 100 )
	
main = runTestTT $ TestList [testGetPrimePairs26, testGetPrimePairs100]
