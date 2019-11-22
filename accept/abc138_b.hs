main :: IO ()
main = do
    n <- readLn :: IO Int
    a <- map read . words <$> getLine :: IO [Double]
    print $ 1 / (sum [ 1.0 / i | i <- a])
