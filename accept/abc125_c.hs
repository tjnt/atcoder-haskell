main :: IO ()
main = do
    n <- readLn :: IO Int
    a <- map read . words <$> getLine :: IO [Int]
    let l = take n $ scanl gcd 0 a
        r = tail $ scanr gcd 0 a
    print . maximum $ zipWith gcd l r
