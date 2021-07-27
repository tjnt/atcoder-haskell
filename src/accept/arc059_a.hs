solve :: [Int] -> Int
solve a = min (f a1) (f a2)
  where
    f t = foldl (\acc i -> acc + (i - t)^2) 0 a
    ceilDiv t s = (t + s - 1) `div` s
    a1 = ceilDiv (sum a) (length a)
    a2 = div (sum a) (length a)

main :: IO ()
main = do
    _ <- getLine
    a <- map read . words <$> getLine :: IO [Int]
    print $ solve a
