isPrime :: Integral a => a -> Bool
isPrime n
  | n <= 2    = n == 2
  | otherwise = odd n && f 3
  where
    f i
      | i^2 > n   = True
      | otherwise = n `rem` i /= 0 && f (i+2) 

main :: IO ()
main = readLn >>= \n -> putStrLn $ if isPrime n then "YES" else "NO"
