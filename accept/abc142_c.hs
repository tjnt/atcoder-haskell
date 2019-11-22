import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List

main :: IO ()
main = do
    n <- readLn :: IO Int
    a <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
    let a' = sortOn fst $ zip a [1..]
    mapM_ (print . snd) a'
