import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import qualified Data.IntMap as M

mapValuesFromList :: [(Int,String)] -> M.IntMap [String]
mapValuesFromList []         = M.empty
mapValuesFromList ((k,v):xs) = M.insertWith (\[a] b -> a:b) k [v]
                             $ mapValuesFromList xs

solve :: [(Int,String)] -> (Int,Int)
solve ps = let w = M.foldl f 0 m
            in (M.size m, w)
  where
    m = M.filter (elem "AC") $ mapValuesFromList ps
    f w vs = w + length (takeWhile (=="WA") vs)

main :: IO ()
main = do
    _ <- getLine
    ps <- map ((\[a,b] -> (read a,b)) . map BS.unpack . BS.words)
        . BS.lines <$> BS.getContents :: IO [(Int,String)]
    let (w,a) = solve ps
    print w
    print a
