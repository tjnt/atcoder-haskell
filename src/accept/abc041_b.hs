modulas = 10^9+7

main :: IO ()
main = do
    [a,b,c] <- map read . words <$> getLine :: IO [Int]
    print $ ((a * b) `mod` modulas * c) `mod` modulas
