main :: IO ()
main = do
    n <- readLn :: IO Int
    a1 <- map read . words <$> getLine :: IO [Int]
    a2 <- map read . words <$> getLine :: IO [Int]
    let s1 = scanl1 (+) a1
        s2 = reverse . scanr1 (+) $ a2
    print $ maximum [ (s1!!i) + s2!!(n-i-1) | i <- [0..n-1] ]
