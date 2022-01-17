import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import qualified Data.Set as S

solve :: [Int] -> [Int]
solve xs = map (`S.findIndex` s) xs
  where
    s = S.fromList xs

main :: IO ()
main = do
    _ <- getLine
    aa <- map (fst . fromJust . BS.readInt)
        . BS.lines <$> BS.getContents :: IO [Int]
    mapM_ print $ solve aa
