main = do
    _ <- getLine
    a <- map read . words <$> getLine :: IO [Integer]
    let m = pred $ foldr1 lcm a
     in print . sum $ map (m `mod`) a
