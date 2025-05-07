module Main where

import Chask
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.List
import Data.Word

-- S1C3 - "Score" text as being in English

c3_in = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

scoreCandidate :: (B.ByteString -> Double) -> (Word8, B.ByteString) -> (Double, Word8, B.ByteString)
scoreCandidate scoreFunc (b, ct) = (scoreFunc ct, b, ct)

{-
cmpFunc :: (Double, Word8, B.ByteString) -> (Double, Word8, B.ByteString) -> Ordering
cmpFunc (a, _, _) (b, _, _)
  | a < b = LT
  | b > a = GT
  | otherwise = EQ
-}

breakSingleByteXor :: (B.ByteString -> Double) -> B.ByteString -> (Double, Word8, B.ByteString)
breakSingleByteXor scoreFunc ct =
  let candidates = map (\b -> (b, xorByte b ct)) [0 .. 0xff]
      scored = map (scoreCandidate scoreFunc) candidates
      sorted = reverse $ sort scored
   in head sorted

c3 = fmap (breakSingleByteXor englishScoreBS) (fromHex c3_in)

main = do
  print $ show c3
