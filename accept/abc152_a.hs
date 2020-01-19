main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    putStrLn $ if n == m then "Yes" else "No"
