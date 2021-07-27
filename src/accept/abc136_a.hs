main = do
    [a,b,c] <- map read . words <$> getLine :: IO [Int]
    print $ c - (min (a - b) c)
