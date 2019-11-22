main :: IO ()
main = do
    [hh,ww] <- map read . words <$> getLine :: IO [Int]
    [h,w] <- map read . words <$> getLine :: IO [Int]
    print $ (hh - h) * (ww - w)
