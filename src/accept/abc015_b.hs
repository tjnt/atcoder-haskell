main :: IO ()
main = do
    _ <- getLine
    aa <- map read . words <$> getLine :: IO [Int]
    let xs = [ a | a <- aa, a /= 0]
        n = length xs
    print $ (sum xs + n - 1) `div` n
