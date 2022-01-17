main :: IO ()
main = do
    [h,a] <- map read . words <$> getLine :: IO [Int]
    print $ (h + a - 1) `div` a
