import Data.List

solve :: [Int] -> [Int] -> Int
solve as bs = if sum d1 * (-1) > sum d2
                 then -1 else chd1 + chd2
  where
    ab = sort . map (uncurry (-)) $ zip as bs
    (d1,d2) = span (<= 0) ab
    chd1 = length . filter (/=0) $ d1
    chd2 = fst . foldl f (0,sum d1 * (-1)) $ reverse d2
    f (c,r) d = if r > 0 then (c+1,max 0 (r-d)) else (c,r)

main :: IO ()
main = do
    _ <- getLine
    a <- map read . words <$> getLine :: IO [Int]
    b <- map read . words <$> getLine :: IO [Int]
    print $ solve a b
