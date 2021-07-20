main :: IO ()
main = do
    m <- readLn :: IO Int
    print' . (`div` 1000) $
        case m of m | m < 100   -> 0
                    | m <= 5000 -> m * 10
                    | m <= 30000 -> m + 50000
                    | m <= 70000 -> (m - 30000) `div` 5 + 80000
                    | otherwise -> 89000
  where
    print' m = let s = show m
                in putStrLn $ if length s == 1 then "0" ++ s else s
