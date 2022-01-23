solve :: Int -> Int -> [Int] -> [Int] -> Int
solve x y aa bb = goA 0 aa bb
  where
    goA k [] _ = 0
    goA k (a:aa) bb
      | k <= a = goB (a+x) aa bb
      | otherwise = goA k aa bb
    goB k _ [] = 0
    goB k aa (b:bb)
      | k <= b = 1 + goA (b+y) aa bb
      | otherwise = goB k aa bb

main :: IO ()
main = do
    _ <- getLine
    [x,y] <- map read . words <$> getLine :: IO [Int]
    aa <- map read . words <$> getLine :: IO [Int]
    bb <- map read . words <$> getLine :: IO [Int]
    print $ solve x y aa bb
