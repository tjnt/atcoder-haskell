main = do
    [a, b, t] <- map read . words <$> getLine
    print $ t `div` a * b
