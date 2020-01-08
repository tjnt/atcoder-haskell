import Data.List

solve :: [Int] -> [Int] -> Int
solve [n,m,l] pqr = maximum . map exp $ permutations pqr
  where
    exp [x,y,z] = (n `div` x) * (m `div` y) * (l `div` z)

main :: IO ()
main = do
    nmr <- map read . words <$> getLine :: IO [Int]
    pqr <- map read . words <$> getLine :: IO [Int]
    print $ solve nmr pqr
