import Data.List

solve e b l
  | n == 6 = 1
  | n == 5 && b `elem` l = 2
  | n == 5 = 3
  | n == 4 = 4
  | n == 3 = 5
  | otherwise = 0
  where
    n = length $ e `intersect` l

main :: IO ()
main = do
    e <- map read . words <$> getLine :: IO [Int]
    b <- readLn :: IO Int
    l <- map read . words <$> getLine :: IO [Int]
    print $ solve e b l
