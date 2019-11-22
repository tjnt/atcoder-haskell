main = do
    [n, k] <- map read . words <$> getLine :: IO [Int]
    print $ k * (k-1) ^ (n-1)
