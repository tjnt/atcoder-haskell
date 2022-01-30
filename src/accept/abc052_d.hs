import Data.List

solve :: Int -> Int -> [Int] -> Int
solve a b xs = snd . foldl' f (head xs, 0) $ tail xs
  where
    f (x1, acc) x2 = (x2, acc + min ((x2-x1) * a) b)

main :: IO ()
main = do
    [_,a,b] <- map read . words <$> getLine :: IO [Int]
    xs <- map read . words <$> getLine :: IO [Int]
    print $ solve a b xs
