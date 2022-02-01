main :: IO ()
main = do
    [x,y,z] <- map read . words <$> getLine :: IO [Int]
    print $ (x - z) `div` (z + y)
