import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Q
import Data.Foldable

solve :: Int -> Int
solve k
  | k < 10 = k
  | otherwise = go (k-1) $ foldl' (|>) Q.empty [1..9]
  where
    go c (x :<| q)
      | c == 0 = x
      | otherwise = 
        let d = x `mod` 10
            ns = [ append x i | i <- [d-1, d, d+1], 0 <= i && i <= 9]
            q' = foldl' (|>) q ns
         in go (c-1) q'
    go _ _ = undefined
    append a b = a * 10 + b

main :: IO ()
main = do
    k <- readLn :: IO Int
    print $ solve k
