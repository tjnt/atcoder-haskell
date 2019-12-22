main :: IO ()
main = do
    [a,b] <- map read . words <$> getLine :: IO [Int]
    print $ lcm a b
