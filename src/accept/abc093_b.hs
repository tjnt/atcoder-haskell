import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List

main :: IO ()
main = do
    [a,b,k] <- map (fst . fromJust . BS.readInt) . BS.words <$> BS.getLine
    let l = [a..(min (a+k-1) b)]
        r = [(max a (b-k+1))..b]
    mapM_ print $ nub (l++r)
