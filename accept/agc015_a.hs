main :: IO ()
main = do
    [n,a,b] <- map read . words <$> getLine :: IO [Int]
    let mn = a * (n-1) + b
        mx = b * (n-1) + a
    print $ max 0 (mx - mn + 1)
