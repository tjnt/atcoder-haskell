import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.List
import Data.Ord

main :: IO ()
main = do
    [_,k] <- map read . words <$> getLine :: IO [Int]
    a <- map (fst . fromJust . BS.readInt)
        . BS.words <$> BS.getLine :: IO [Int]
    let g = sortOn Down . map length . group . sort $ a
    print $ sum . drop k $ g
