import Data.List
import Data.Array.IArray

solve :: Int -> [Int] -> Double
solve k p = maximum $ map (\i -> (pp!i) - (pp!(i-k))) [k..n]
  where
    n = length p
    f x = (fromIntegral x + 1) / 2
    pp = listArray (0,n) . scanl (+) 0 $ map f p :: Array Int Double

main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Int]
    p <- map read . words <$> getLine :: IO [Int]
    print $ solve k p
