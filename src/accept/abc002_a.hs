main :: IO ()
main = do
    [x,y] <- map read . words <$> getLine :: IO [Int]
    print $ max x y
