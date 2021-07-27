import Data.List
import Data.Maybe

solve :: [Int] -> [Int] -> [Int] -> Int
solve n p q = abs $ a - b
  where
    np = sort $ permutations n
    a = succ . fromJust $ elemIndex p np
    b = succ . fromJust $ elemIndex q np

main :: IO ()
main = do
    n <- readLn :: IO Int
    p <- map read . words <$> getLine :: IO [Int]
    q <- map read . words <$> getLine :: IO [Int]
    print $ solve [1..n] p q
