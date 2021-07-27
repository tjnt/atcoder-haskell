import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List

main :: IO ()
main = do
    [n,x0] <- map read . words <$> getLine :: IO [Int]
    x <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
    let xs = sort (x0:x)
    print . foldr1 gcd . tail $ zipWith (-) xs (0:xs)
