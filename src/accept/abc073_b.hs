import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

main :: IO ()
main = do
    _ <- getLine
    xs <- map ((\[a,b] -> (a,b)) . map (fst . fromJust . BS.readInt) . BS.words)
        . BS.lines <$> BS.getContents :: IO [(Int,Int)]
    print . sum $ map (\t -> snd t - fst t + 1) xs
