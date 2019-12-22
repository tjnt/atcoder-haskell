main :: IO ()
main = do
    n <- readLn :: IO Int
    a <- map read . words <$> getLine :: IO [Int]
    let r = fst $ foldl f (0,1) a
    print $ if r == n then -1 else r
  where
    f (c,i) a = if i == a
                   then (c,i+1)
                   else (c+1,i)
