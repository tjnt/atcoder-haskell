import Data.Array.IArray
import Data.List (sort)
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

lowerBound:: (Integral i, Ix i, Ord e, IArray a e) =>
                e -> a i e -> i
lowerBound x a =
    let (b,e) = bounds a in bsearch b (e + 1)
  where
    bsearch b e
      | e == b    = b
      | x <= a!p  = bsearch b p
      | otherwise = bsearch (p + 1) e
      where p = (b + e) `div` 2

upperBound:: (Integral i, Ix i, Ord e, IArray a e) =>
                e -> a i e -> i
upperBound x a =
    let (b,e) = bounds a in bsearch b (e + 1)
  where
    bsearch b e
      | e == b    = b
      | x < a!p   = bsearch b p
      | otherwise = bsearch (p + 1) e
      where p = (b + e) `div` 2

main :: IO ()
main = do
    n <- readLn :: IO Int
    [a,b,c] <- map (map (fst . fromJust . BS.readInt) . BS.words) . BS.lines <$> BS.getContents
    let a' = listArray (0,n-1) (sort a) :: Array Int Int
    let c' = listArray (0,n-1) (sort c) :: Array Int Int
    print $ sum [ lowerBound i a' * (n - upperBound i c')
                | i <- b ]
