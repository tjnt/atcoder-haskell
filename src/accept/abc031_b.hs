main :: IO ()
main = do
    [l,h] <- map read . words <$> getLine :: IO [Int]
    _ <- getLine
    aa <- map read . lines <$> getContents :: IO [Int]
    let f a | a < l = l - a
            | h < a = -1
            | otherwise = 0
    mapM_ (print . f) aa
