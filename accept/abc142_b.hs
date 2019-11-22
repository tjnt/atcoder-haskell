import qualified Data.ByteString.Char8 as BS
import Data.Maybe

main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Int]
    h <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
    print . length $ filter (>=k) h
