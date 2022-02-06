main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Int]
    let a = n `mod` k
        b = abs (k - a)
    print $ min a b
