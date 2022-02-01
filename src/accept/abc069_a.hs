main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    print $ pred n * pred m
