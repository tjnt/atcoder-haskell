main = do
    [n,k,x,y] <- map read . lines <$> getContents
    print $ min n k * x + max (n - k) 0 * y
