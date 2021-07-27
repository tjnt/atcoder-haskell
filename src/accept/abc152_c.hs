import Data.List

solve :: [Int] -> Int
solve a = sum [ 1 | (i,j) <- zip a mn, i <= j ]
  where
    mn = scanl1 min a

main :: IO ()
main = do
    _ <- getLine
    a <- map read . words <$> getLine :: IO [Int]
    print $ solve a
