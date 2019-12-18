solve :: Int -> Int -> Float
solve n m = min d (360 - d)
  where
    m' = fromIntegral (6 * m) :: Float
    n' = fromIntegral (30 * (n `mod` 12))
       + 0.5 * fromIntegral m :: Float
    d = abs $ m' - n'

main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    print $ solve n m
