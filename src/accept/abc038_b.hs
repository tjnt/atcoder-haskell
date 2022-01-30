main :: IO ()
main = do
    [h1,w1] <- map read . words <$> getLine :: IO [Int]
    [h2,w2] <- map read . words <$> getLine :: IO [Int]
    let res | h1 == h2 = True
            | h1 == w2 = True
            | w1 == h2 = True
            | w1 == w2 = True
            | otherwise = False
    putStrLn $ if res then "YES" else "NO"
