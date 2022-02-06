import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))

solve :: Int -> Int -> [Int] -> Int
solve n k xs
  | V.elem 0 v = n
  | otherwise = go 0 (v!0) 0 0
  where
    v = V.fromList xs
    go c p l r
      | l == n = c
      | r == n = moveL
      | p <= k = moveR
      | l < r  = moveL
      | l == r = moveLR
      | otherwise = undefined
      where
        moveR  = go c (p*(v!(r+1))) l (r+1)
        moveL  = go (max c (r-l)) (p`div`(v!l)) (l+1) r
        moveLR = go (max c 0) (v!(l+1)) (l+1) (r+1)

main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Int]
    xs <- map read . lines <$> getContents :: IO [Int]
    print $ solve n k xs
