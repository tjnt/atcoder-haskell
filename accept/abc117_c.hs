import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.List
import Data.Ord

distance [] = []
distance [x] = []
distance (x1:x2:xs) = abs (x1 - x2) : distance (x2:xs)

main :: IO ()
main = do
    [n,_] <- map read . words <$> getLine :: IO [Int]
    x <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
    print . sum . drop (n-1) . sortOn Down . distance . sort $ x
