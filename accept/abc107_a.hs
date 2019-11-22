main :: IO ()
main = do
    [n,i] <- map read . words <$> getLine :: IO [Int]
    print $ n - i + 1
