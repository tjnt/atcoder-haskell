import Data.List

solve :: Int -> [Int] -> Int
solve n = f . sort
  where
    f [a,b,c]
      | a == b && b == c = n
      | a == b = solve (n+1) [a+1, b+1, c]
      | otherwise = solve (n+1) [a+2, b, c]

main :: IO ()
main = do
    xs <- map read . words <$> getLine :: IO [Int]
    print $ solve 0 xs
