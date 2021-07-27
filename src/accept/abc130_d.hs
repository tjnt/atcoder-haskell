import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.List
import Data.Array.IArray

lowerBound:: (Integral i, Ix i, Ord e, IArray a e) => e -> a i e -> i
lowerBound x a =
    let (b,e) = bounds a in bsearch b (e + 1)
  where
    bsearch b e
      | e == b    = b
      | x <= a!p  = bsearch b p
      | otherwise = bsearch (p + 1) e
      where p = (b + e) `div` 2

solve :: Int -> [Int] -> Int
solve k a = go (0:cs)
  where
    n = length a
    cs = scanl1 (+) a
    a' = listArray (1,n) cs :: Array Int Int
    go [] = 0
    go (x:xs) = (n-i+1) + go xs
      where
        i = lowerBound (k+x) a'

main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Int]
    a <- map (fst . fromJust . BS.readInt)
        . BS.words <$> BS.getLine :: IO [Int]
    print $ solve k a
