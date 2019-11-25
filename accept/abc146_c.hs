import Data.Int
import Control.Monad

keta :: Integral a => a -> a
keta 0 = 0
keta n = 1 + keta (n `div` 10)

calc :: Int64 -> Int64 -> Int64 -> Int64 -> Int64
calc a b x n = a * n + b * keta n

bisectionMethod :: Integral a => (a -> Bool) -> (a,a) -> a
bisectionMethod f (l,h)
  | h - l == 1 = l
  | f m        = bisectionMethod f (m,h)
  | otherwise  = bisectionMethod f (l,m)
  where
    m = l + (h-l) `div` 2

main :: IO ()
main = do
    [a,b,x] <- map read . words <$> getLine :: IO [Int64]
    let f n = calc a b x n <= x
    print $ bisectionMethod f (0, 10^9+1)
