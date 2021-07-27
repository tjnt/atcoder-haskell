main :: IO ()
main = do
    [n,m,c] <- map read . words <$> getLine :: IO [Int]
    b <- map read . words <$> getLine :: IO [Int]
    a <- map (map read . words) . lines <$> getContents :: IO [[Int]]
    let a' = map ((+c) . sum . zipWith (*) b) a
    print . length $ filter (0<) a'
