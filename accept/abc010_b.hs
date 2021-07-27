solve :: [Int] -> Int
solve [] = 0
solve (x:xs)
  | x `rem` 2  == 0 = 1 + solve xs'
  | x `rem` 3  == 2 = 1 + solve xs'
  | otherwise = solve xs
  where
    xs' = (x-1):xs

main :: IO ()
main = do
    _ <- readLn :: IO Int
    a <- map read . words <$> getLine :: IO [Int]
    print $ solve a
