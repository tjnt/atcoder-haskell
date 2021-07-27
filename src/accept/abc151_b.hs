main :: IO ()
main = do
    [n,k,m] <- map read . words <$> getLine :: IO [Int]
    a <- map read . words <$> getLine :: IO [Int]
    let x = m * n - sum a
    print $ case m * n - sum a of
        x | x > k     -> -1
          | x < 0     -> 0
          | otherwise -> x
