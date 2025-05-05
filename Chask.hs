module Chask
  ( toHex,
    fromHex,
    toB64,
    xorBS,
  )
where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as BSB64
import qualified Data.ByteString.Char8 as C8
import Data.List.Split
import Data.Word
import Numeric

-- import Text.ParserCombinators.ReadP (string)

-- S1C1 - convert hex -> data, then data -> base64
c1_in = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

c1_expected = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

{-

General approach:
    - Use `Either String x` for error reporting handling
        - Consider an error type?
    - Use 'String' for hex strings, base64 strings
        - Consider defining newtypes such that we can't confuse Hex and B64 strings
    - Use ByteString.Strict for binary data

-}

-- There _must_ be a better way than this :-)
hexDigitToNum :: Char -> Either String Word8
hexDigitToNum '0' = Right 0
hexDigitToNum '1' = Right 1
hexDigitToNum '2' = Right 2
hexDigitToNum '3' = Right 3
hexDigitToNum '4' = Right 4
hexDigitToNum '5' = Right 5
hexDigitToNum '6' = Right 6
hexDigitToNum '7' = Right 7
hexDigitToNum '8' = Right 8
hexDigitToNum '9' = Right 9
hexDigitToNum 'a' = Right 10
hexDigitToNum 'b' = Right 11
hexDigitToNum 'c' = Right 12
hexDigitToNum 'd' = Right 13
hexDigitToNum 'e' = Right 14
hexDigitToNum 'f' = Right 15
hexDigitToNum c = Left ("Bad hex digit: " ++ [c])

pairToByte :: String -> Either String Word8
pairToByte [a, b] = do
  x <- hexDigitToNum a
  y <- hexDigitToNum b
  return $ x * 16 + y
pairToByte l = Left "Can only handle two-element hex string"

stringToPairs :: String -> Either String [String]
stringToPairs s = if even (length s) then Right (chunksOf 2 s) else Left ("Odd length list: " ++ show (length s))

fromHex :: String -> Either String B.ByteString
fromHex hs =
  fmap B.pack (stringToPairs hs >>= mapM pairToByte)

toB64 :: B.ByteString -> String
toB64 bs = C8.unpack $ BSB64.encode bs

toHex :: B.ByteString -> String
toHex bs = concatMap (`showHex` "") (B.unpack bs)

xorBS :: B.ByteString -> B.ByteString -> Either String B.ByteString
xorBS a b =
  let la = B.length a
      lb = B.length b
   in if la /= lb
        then
          Left ("Length mismatch: " ++ show la ++ " != " ++ show lb)
        else
          Right (B.pack (zipWith xor (B.unpack a) (B.unpack b)))
