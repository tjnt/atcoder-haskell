solve n m = (100 * (n-m) + 1900 * m) * 2 ^ m

main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    print $ solve n m
