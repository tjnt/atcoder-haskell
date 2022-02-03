import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.List
import qualified Data.Vector as V
import Data.Vector ((!))

absmin :: Int -> Int -> Int
absmin a b = min (abs a) (abs b)

solve :: Int -> Int -> [Int] -> Int
solve n k xs = minimum $ map f [0..n-k]
  where
    v = V.fromList xs
    f i = let l = v!i
              r = v!(i+k-1)
           in r - l + absmin l r

main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Int]
    xx <- map (fst . fromJust . BS.readInt)
        . BS.words <$> BS.getLine :: IO [Int]
    print $ solve n k xx
