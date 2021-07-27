main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Double]
    print $ sum [
        let x = ceiling $ logBase 2 (k / i)
         in if i < k then (1.0 / n) * ((1.0 / 2.0) ^ x) else 1.0 / n
        | i <- [1..n] ]
