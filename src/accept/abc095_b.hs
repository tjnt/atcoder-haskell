main :: IO ()
main = do
    [n,x] <- map read . words <$> getLine :: IO [Int]
    m <- map read . lines <$> getContents :: IO [Int]
    let x' = x - sum m
    print $ n + (x' `div` minimum m)
