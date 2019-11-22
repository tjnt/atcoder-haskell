import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List

main :: IO ()
main = do
    n <- readLn :: IO Int
    a <- map (map (fst . fromJust . BS.readInt) . BS.words) . BS.lines <$> BS.getContents
    print . length . filter odd . map length . group . sort $ a
