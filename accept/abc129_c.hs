import           Data.Array.IArray
import           Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set

modN = 1000000007 :: Integer

main :: IO ()
main = do
    [n,_] <- map read . words <$> getLine :: IO [Int]
    a <- Set.fromList . map (fst . fromJust . BS.readInt) . BS.lines <$> BS.getContents
    print $ solve n a `mod` modN

solve :: Int -> Set.Set Int -> Integer
solve n a = dp!n
  where
    dp = listArray (0,n) (map f [0..n]) :: Array Int Integer
    f k
      | Set.member k a = 0
      | k == 0 = 1
      | k == 1 = 1
      | otherwise = dp!(k-1) + dp!(k-2)
