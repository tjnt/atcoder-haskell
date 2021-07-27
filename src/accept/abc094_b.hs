main :: IO ()
main = do
    [n,_,x] <- map read . words <$> getLine :: IO [Int]
    a <- map read . words <$> getLine :: IO [Int]
    let a' = map (\i -> if i `elem` a then 1 else 0) [0..n]
        (l,r) = splitAt x a'
    print $ min (sum l) (sum r)
