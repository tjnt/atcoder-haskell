import Data.Int

modulus :: Int64
modulus = 10^9 + 7

fact :: Int64 -> Int64
fact n = f n 1
  where
    f 0 b = b
    f a b = b `seq` f (a - 1) ((a * b) `rem` modulus)

solve :: Int64 -> Int64 -> Int64
solve n m = calc `rem` modulus
  where
    calc
      | n == m           = 2 * fact n * fact m
      | abs (n - m) == 1 = fact n * fact m
      | otherwise        = 0

main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int64]
    print $ solve n m
