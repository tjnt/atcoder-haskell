main = do
    [n, d] <- map read . words <$> getLine :: IO [Int]
    print $ minimum [i | i <- [1..n], (2 * d + 1) * i >= n]
