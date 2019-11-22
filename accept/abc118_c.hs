import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.List

main :: IO ()
main = do
    _ <- getLine
    a <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
    print $ foldr1 gcd a
