solve :: Int -> Int -> [(String,Int)] -> Int
solve a b = go
  where
    go [] = 0
    go ((s,d):xs)
      | s == "East" = d' + go xs
      | s == "West" = negate d' + go xs
      where
        d' | d < a = a
           | d > b = b
           | otherwise = d

main :: IO ()
main = do
    [n,a,b] <- map read . words <$> getLine :: IO [Int]
    sds <- map ((\[a,b] -> (a, read b)) . words)
        . lines <$> getContents :: IO [(String,Int)]
    let res = solve a b sds
    putStrLn $ case solve a b sds of
        res | res < 0 -> "West " ++ (show . abs) res
            | res > 0 -> "East " ++ show res
            | otherwise -> show 0
