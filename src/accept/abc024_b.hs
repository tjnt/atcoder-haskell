solve :: Int -> [Int] -> Int
solve t xs = go xs
  where
    go [_] = t
    go (x:y:xs) = min (x + t) y - x + go (y:xs)

main :: IO ()
main = do
    [n,t] <- map read . words <$> getLine :: IO [Int]
    aa <- map read . lines <$> getContents :: IO [Int]
    print $ solve t aa
