main :: IO ()
main = do
    [w,a,b] <- map read . words <$> getLine :: IO [Int]
    print $ case w of
        _ | a + w < b -> b - a - w
          | b + w < a -> a - b - w
          | otherwise -> 0
