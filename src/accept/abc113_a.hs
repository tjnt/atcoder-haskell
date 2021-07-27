main :: IO ()
main = do
    [x,y] <- map read . words <$> getLine :: IO [Int]
    print $ x + y `div` 2
