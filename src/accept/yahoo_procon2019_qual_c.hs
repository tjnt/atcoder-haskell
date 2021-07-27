solve :: Int -> Int -> Int -> Int
solve k a b = a + (b - a) * d + m
  where
    (d,m) = (k - a + 1) `divMod` 2

main :: IO ()
main = do
    [k,a,b] <- map read . words <$> getLine :: IO [Int]
    print $ if a + 2 < b
               then solve k a b else k + 1
