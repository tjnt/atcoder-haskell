main :: IO ()
main = do
    [n,tt] <- map read . words <$> getLine :: IO [Int]
    ct <- map ((\[a,b] -> (a,b)) . (map read . words))
        . lines <$> getContents :: IO [(Int,Int)]
    let lst = [ c | (c,t) <- ct, t <= tt ]
    putStrLn $ if null lst then "TLE" else show $ minimum lst
