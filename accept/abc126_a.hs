import           Data.Char
main = do
    [_, k] <- map read . words <$> getLine
    s <- getLine
    putStrLn $ f k 1 s
  where
    f :: Int -> Int -> String -> String
    f _ _ [] = []
    f k n (x:xs) =
        if k == n
           then toLower x : f k (n+1) xs
           else x : f k (n+1) xs

