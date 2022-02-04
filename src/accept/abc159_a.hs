main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    print $ (n * (n-1) + m * (m-1)) `div` 2
