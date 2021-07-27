main :: IO ()
main = do
    n <- readLn :: IO Int
    a <- map read . words <$> getLine :: IO [Int]
    b <- map read . words <$> getLine :: IO [Int]
    c <- map read . words <$> getLine :: IO [Int]
    print $ sum [ let ai = a!!(i-1)
                      bi = b!!(ai-1)
                      ci = if i /= 1 && ai == (a!!(i-2) + 1)
                              then c!!(ai-2) else 0
             in bi + ci | i <- [1..n] ] 
