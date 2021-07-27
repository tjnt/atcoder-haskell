main :: IO ()
main = do
    [k,x] <- map read . words <$> getLine :: IO [Int]
    putStrLn $ if 500*k >= x then "Yes" else "No"
