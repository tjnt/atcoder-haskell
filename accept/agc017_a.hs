solve :: Int -> [Int] -> Int
solve p a
  | all even a = if p == 0 then c else 0
  | otherwise  = c `div` 2
  where
    n = length a
    c = 2^n

main :: IO ()
main = do
    [_,p] <- map read . words <$> getLine :: IO [Int]
    a <- map read . words <$> getLine :: IO [Int]
    print $ solve p a
