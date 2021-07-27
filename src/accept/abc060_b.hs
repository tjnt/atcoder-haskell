main :: IO ()
main = do
    [a,b,c] <- map read . words <$> getLine :: IO [Int]
    let ret = c `elem` [a * i `rem` b | i <- [1..b]]
    putStrLn $ if ret then "YES" else "NO"
