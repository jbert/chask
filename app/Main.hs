module Main where

-----------------------------------

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

main :: IO ()
main = do
  let fibs = fib <$> [0..10]
  print fibs
  putStrLn "All done."

-- End ---------------------------------------------------------------
