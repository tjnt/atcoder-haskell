main = do
    [a, b] <- map read . words <$> getLine
    print $ (a + b) `mod` 24
