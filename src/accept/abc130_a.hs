main = do
    [x, a] <- map read . words <$> getLine :: IO [Int]
    putStrLn $ if x < a then "0" else "10"
