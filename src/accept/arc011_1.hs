solve x m n
  | x < m = 0
  | otherwise = let x' = x - m + n in n + solve x' m n

main :: IO ()
main = do
    [m,n,nn] <- map read . words <$> getLine :: IO [Int]
    print $ nn + solve nn m n
