main :: IO ()
main = do
    n <- readLn :: IO Int
    x <- map read . words <$> getLine :: IO [Int]
    let p1 = sum x `div` n
        p2 = sum x `div` n + 1
        f p a i = a + (i - p) ^ 2
    print $ min (foldl (f p1) 0 x) (foldl (f p2) 0 x)
