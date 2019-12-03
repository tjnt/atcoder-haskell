main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    let m' = m `div` 2
    print $ if n < m'
               then n + ((m - 2 * n) `div` 4)
               else min n m'
