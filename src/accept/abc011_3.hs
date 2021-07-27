import Data.List

solve :: Int -> [Int] -> Bool
solve n ng
  | n `elem` ng = False
  | otherwise = let c = go 0 n in c <= 100
  where
    f  x = x >= 0 && x `notElem` ng
    go c 0 = c
    go c x = maybe 200 (go (c+1)) $ find f [x-3,x-2,x-1]

main :: IO ()
main = do
    n <- readLn :: IO Int
    ng <- filter (n >=) . map read . lines <$> getContents :: IO [Int]
    putStrLn $ if solve n ng then "YES" else "NO"
