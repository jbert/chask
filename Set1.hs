module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as BSB64
import Data.Either
import Data.Function
import Numeric (readHex)
import Test.HUnit

c1input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

c1expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

-- convert hex parse to Either fail/succeed
processReadS :: [(a, String)] -> Either String a
processReadS [] = Left "No parse"
processReadS [(v, "")] = Right v
processReadS [(v, s)] = Left ("Incomplete parse: " ++ s)
processReadS l = Left ("Multiple matches: " ++ show (length l))

dehex :: String -> Either String B.ByteString
dehex s = readHex s & processReadS

main = do
  print "hello"

{-
c1got = dehex c1input & BSB64.encode

assertNoErr e = TestCase (assertEqual "No error" True (isRight e))

c1 (Left e) = TestCase (assertEqual ("Error" ++ show e) True False)
c1 (Right got) = TestCase (assertEqual "Correct val" c1expected got)

-- f :: Either String Integer -> Assertion
-- f (Left e) = assertFail ("Error: " ++ e)
-- f (Right s) = assertEqual "Correct Val" s c1expected

-- tests = TestList [TestLabel "No err" (assertNoErr c1got), TestLabel "Correct val" c1]
-- tests = TestList [TestLabel "No err" (assertNoErr c1got)]
tests = TestList [TestLabel "C1" (c1 c1got)]

main = runTestTTAndExit tests
-}
