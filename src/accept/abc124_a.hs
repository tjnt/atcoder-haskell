main = do
    [a,b] <- map read . words <$> getLine
    print $ max a b + max (max a b - 1) (min a b)
