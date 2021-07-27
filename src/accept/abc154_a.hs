main :: IO ()
main = do
    [s,t] <- words <$> getLine
    [a,b] <- map read . words <$> getLine :: IO [Int]
    u <- getLine
    mapM_ print $ if u == s then [a-1,b] else [a,b-1]
