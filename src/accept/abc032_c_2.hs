import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))
import qualified Data.Sequence as Q
import Data.Foldable

solve :: Int -> Int -> [Int] -> Int
solve n k xs
  | 0 `elem` xs = n
  | otherwise = go 0 1 Q.empty xs
  where
    go c p ls [] = case ls of
        Q.Empty -> c
        l Q.:<| ls' -> go (max c (Q.length ls)) (p`div`l) ls' []
    go c p ls (r:rs)
      | p * r <= k = go c (p*r) (ls Q.|> r) rs
      | otherwise = case ls of
          Q.Empty -> go c 1 Q.empty rs
          l Q.:<| ls' -> go (max c (Q.length ls)) (p`div`l) ls' (r:rs)

main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Int]
    xs <- map (fst . fromJust . BS.readInt)
        . BS.lines <$> BS.getContents :: IO [Int]
    print $ solve n k xs
