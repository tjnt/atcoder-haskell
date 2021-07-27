main :: IO ()
main = do
    d <- map read . words <$> getLine :: IO [Int]
    j <- map read . words <$> getLine :: IO [Int]
    print . sum $ zipWith max d j
