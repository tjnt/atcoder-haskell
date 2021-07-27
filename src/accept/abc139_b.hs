main :: IO ()
main = do
    [a,b] <- map read . words <$> getLine :: IO [Int]
    let c = if (b - a) `mod` (a - 1) == 0 then 0 else 1
    print $ (b - a) `div` (a - 1) + 1 + c
