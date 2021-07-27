import qualified Data.ByteString.Char8 as BS
import Data.Maybe

main :: IO ()
main = do
    _ <- getLine
    lr <- map ((\[a,b] -> (a,b)) . map (fst . fromJust . BS.readInt) . BS.words) . BS.lines <$> BS.getContents
    let l = maximum $ map fst lr
        r = minimum $ map snd lr
     in print $ max (r - l + 1) 0
