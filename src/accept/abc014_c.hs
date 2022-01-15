import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import Data.Array.IArray
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

solve :: [(Int,Int)] -> Int
solve xs = maximum . scanl1 (+) $ xs'
  where
    xs' = elems $ runSTArray $ do
        m <- newArray (0,1000001) 0 :: ST s (STArray s Int Int)
        forM_ xs $ \(a,b) -> do
            s <- readArray m a
            writeArray m a (s+1)
            e <- readArray m (b+1)
            writeArray m (b+1) (e-1)
        return m

main :: IO ()
main = do
    _ <- getLine :: IO String
    xs <- map ((\[a,b] -> (a,b)) . map (fst . fromJust . BS.readInt) . BS.words)
        . BS.lines <$> BS.getContents :: IO [(Int,Int)]
    print $ solve xs
