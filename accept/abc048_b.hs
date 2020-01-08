main :: IO ()
main = do
    [a,b,x] <- map read . words <$> getLine :: IO [Int]
    print $ b `div` x - (a-1) `div` x
