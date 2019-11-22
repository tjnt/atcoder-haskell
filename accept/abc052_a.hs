main = do
    [a, b, c, d] <- map read . words <$> getLine
    print $ maximum [ a * b, c * d]
