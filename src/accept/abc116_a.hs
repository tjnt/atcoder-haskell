main :: IO ()
main = do
    [a,b,c] <- map read . words <$> getLine :: IO [Int]
    print $ a * b `div` 2
