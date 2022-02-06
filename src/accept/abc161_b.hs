ceilDiv t s = (t + s - 1) `div` s

main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    as <- map read . words <$> getLine :: IO [Int]
    let s = sum as
        c = length [ a | a <- as, a >= (s `ceilDiv` (4 * m)) ]
    putStrLn $ if m <= c then "Yes" else "No"
