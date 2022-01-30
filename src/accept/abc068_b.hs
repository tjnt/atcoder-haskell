import Data.List

divable2 :: Int -> Int
divable2 n
  | even n = 1 + divable2 (n `div` 2)
  | otherwise = 0

solve :: Int -> Int
solve n = fst $ foldl' f (1,0) [1..n]
  where
    f (i,a) j
      | divable2 j > a = (j, divable2 j)
      | otherwise = (i,a)

main :: IO ()
main = do
    n <- readLn :: IO Int
    print $ solve n
