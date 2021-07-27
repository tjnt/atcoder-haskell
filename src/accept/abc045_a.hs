main = do
    [a, b, h] <- map read . words <$> getContents
    print ((a + b) * h `div` 2)
