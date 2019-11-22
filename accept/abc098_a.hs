main :: IO ()
main = do
    [a,b] <- map read . words <$> getLine :: IO [Int]
    print $ maximum [a+b,a-b,a*b]
