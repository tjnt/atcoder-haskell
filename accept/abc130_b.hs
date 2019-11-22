main = do
    [n, x] <- map read . words <$> getLine
    l <- map read . words <$> getLine
    print . length . filter (x>=) . reverse . foldl (\a x -> (head a + x):a) [0] $ l
