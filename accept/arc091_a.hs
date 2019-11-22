main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    print $ f n * f m
  where
    f x = if x < 2 then x else x - 2
