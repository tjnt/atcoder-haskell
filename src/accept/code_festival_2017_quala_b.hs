solve :: Int -> Int -> Int -> Bool
solve n m k = elem k $ do
    a <- [0..n]
    f a <$> [0..m]
  where
    f a b = m * a + n * b - a * b * 2

main :: IO ()
main = do
    [n,m,k] <- map read . words <$> getLine :: IO [Int]
    putStrLn $ if solve n m k then "Yes" else "No"
