main :: IO ()
main = do
    [h,n] <- map read . words <$> getLine :: IO [Int]
    aa <- map read . words <$> getLine :: IO [Int]
    putStrLn $ if sum aa >= h then "Yes" else "No"
