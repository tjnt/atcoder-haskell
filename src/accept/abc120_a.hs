main :: IO ()
main = do
    [a,b,c] <- map read . words <$> getLine :: IO [Int]
    print $ if a * c < b then c else b `div` a
