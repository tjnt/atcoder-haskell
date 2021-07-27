digitNum n = go 0
  where
    go i 0 = i
    go i x = go (i+1) (x `div` n)


main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Int]
    print $ digitNum k n
