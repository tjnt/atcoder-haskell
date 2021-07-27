main = do
    [a, b, c] <- map read . words <$> getLine
    putStrLn $ if b - a == c - b then "YES" else "NO"
