module AGPP_Test where

import AGPP( getPrimePairs )
import Test.HUnit

testGetPrimePairs = TestCase $ assertEqual
	"getPrimePairs failed for n = 26" [(3,23),(7,19),(13,13)] ( getPrimePairs 26 )
	
main = runTestTT $ TestList [testGetPrimePairs]
