module Main where

import Chask
import Test.HUnit

{-

General approach:
    - Use `Either String x` for error reporting handling
        - Consider an error type?
    - Use 'String' for hex strings, base64 strings
        - Consider defining newtypes such that we can't confuse Hex and B64 strings
    - Use ByteString.Strict for binary data

-}

-- S1C1 - convert hex -> data, then data -> base64
c1_in = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

c1_expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

c1_got = fmap toB64 (fromHex c1_in)

-- There is likely a better way to assert we didn't error?
c1_test (Left e) = TestCase (assertEqual ("Error" ++ show e) True False)
c1_test (Right got) = TestCase (assertEqual "Correct val" c1_expected got)

-- S1C2 - XOR two hex buffers, show as hex
c2_in_a = "1c0111001f010100061a024b53535009181c"

c2_in_b = "686974207468652062756c6c277320657965"

c2_expected = "746865206b696420646f6e277420706c6179"

-- Trying to do this without `do` syntax was breaking my brain
c2_got :: Either String String
c2_got = do
  a <- fromHex c2_in_a
  b <- fromHex c2_in_b
  c <- xorBS a b
  return $ toHex c

-- Find nicer way to assert against Either and/or reduce boilerplate
c2_test (Left e) = TestCase (assertEqual ("Error" ++ show e) True False)
c2_test (Right got) = TestCase (assertEqual "Correct val" c2_expected got)

set1_tests =
  TestList
    [ TestLabel "C1" (c1_test c1_got),
      TestLabel "C2" (c2_test c2_got)
    ]

main = runTestTTAndExit set1_tests
