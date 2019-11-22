-- これは解説を見て解いた
main = do
    [l,r] <- map read . words <$> getLine :: IO [Int]
    let r' = min r (l+2019*2)
    print $ minimum [ i * j `mod` 2019 | i <- [l..r'], j <- [i+1..r'] ]
