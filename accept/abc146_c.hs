import Data.Int
import Control.Monad

keta :: Integral a => a -> a
keta 0 = 0
keta n = 1 + keta (n `div` 10)

calc :: Int64 -> Int64 -> Int64 -> Int64 -> Int64
calc a b x n = a * n + b * keta n

bsearch :: Integral a => (a -> a) -> a -> (a, a) -> a
bsearch f t (l, r)  =
    case f c of
        x | x == t -> c
          | l == c && c == r -> c
          | x > t  -> bsearch f t (l, c-1)
          | x < t  -> bsearch f t (c+1, r)
  where
    c = (l + r) `div` 2

main :: IO ()
main = do
    [a,b,x] <- map read . words <$> getLine :: IO [Int64]
    let n = bsearch (calc a b x) x (0, 10^9)
     in print $ if calc a b x n > x then n - 1 else n
