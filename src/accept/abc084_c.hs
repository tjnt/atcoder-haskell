import Data.Array.IArray

solve :: Int -> Array Int (Int,Int,Int) -> [Int]
solve n a = map (go 0) [1..n]
  where
    go :: Int -> Int -> Int
    go t i
      | i == n = t
      | t <= s = go (s+c) (i+1)
      | otherwise = let k = (t + f - 1) `div` f
                     in go (k * f + c) (i+1)
      where
        (c,s,f) = a!i

main :: IO ()
main = do
    n <- readLn :: IO Int
    a <- listArray (1,n-1) . map ((\[a,b,c] -> (a,b,c)) . (map read . words))
       . lines <$> getContents :: IO (Array Int (Int,Int,Int))
    mapM_ print $ solve n a
