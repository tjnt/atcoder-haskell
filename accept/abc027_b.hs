import Data.List

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn t xs =
    case break (==t) xs of
        (a,[])  -> [a]
        (a,[_]) -> a:[[]]
        (a,_:b) -> a : splitOn t b

solve :: [Int] -> Int
solve xs
  | m /= 0 = -1
  | otherwise = foldl f 0 . splitOn 0
              . scanl (+) 0 $ map (d-) xs
  where
    (d,m) = sum xs `divMod` length xs
    f a v  = a + length v

main = getLine >> getLine >>=
       (print . solve . map read . words)
