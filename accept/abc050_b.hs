import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Control.Monad

main :: IO ()
main = do
    _ <- getLine
    t <- zip [1..] <$> map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
    _ <- getLine
    px <- map ((\[a,b] -> (a,b)) . map (fst . fromJust . BS.readInt) . BS.words) . BS.lines <$> BS.getContents
    let f (p,x) = foldr (\(i,t) j -> if i == p then j+x else j+t)
    forM_ px $ \p -> print $ f p 0 t
