main :: IO ()
main = do
    [s,t] <- map read . words <$> getLine :: IO [Int]
    print $ t - s + 1
