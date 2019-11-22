ceilDiv :: Integral a => a -> a -> a
ceilDiv t s = (t + s - 1) `div` s

main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Int]
    print $ ((n - k) `ceilDiv` (k - 1)) + 1
