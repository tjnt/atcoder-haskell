main = do
    [a,b,c,d] <- map read . words <$> getLine :: IO [Int]
    let f x = x - (x `div` c + x `div` d - (x `div` (c * d `div` gcd c d)))
    print $ f b - f (a-1)
