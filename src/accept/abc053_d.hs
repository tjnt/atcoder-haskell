import Data.List

solve :: [Int] -> Int
solve xs
  | odd n = n
  | otherwise = n - 1
  where
    n = length . group $ sort xs

main :: IO ()
main = do
    _ <- getLine
    xs <- map read . words <$> getLine :: IO [Int]
    print $ solve xs
