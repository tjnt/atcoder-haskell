main :: IO ()
main = do
    n <- readLn :: IO Int
    k <- readLn :: IO Int
    x <- map read . words <$> getLine :: IO [Int]
    print . sum $ map (\i -> 2 * min i (k-i)) x
