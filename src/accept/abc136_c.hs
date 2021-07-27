main = do
    getLine
    (h1:h) <- map read . words <$> getLine :: IO [Int]
    putStrLn $ if f h1 h then "Yes" else "No"
  where
    f m []   = True
    f m (hi:h) =
        if m <= hi then f (max m hi) h
        else if (m-1) <= hi then f (max m hi) h
        else False
