main :: IO ()
main = do
    [a,b] <- map read . words <$> getLine :: IO [Int]
    let d = b - a
        r = sum [d,d-1..1]
    print $ r - b
