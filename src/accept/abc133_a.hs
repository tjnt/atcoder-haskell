main = do
    [n,a,b] <- map read . words <$> getLine
    print $ min (n*a) b
