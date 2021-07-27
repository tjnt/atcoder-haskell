import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.Array.IArray
import Data.List (nub)

type Vertex a  = a
type Graph a   = Array (Vertex a) [Vertex a]
type Bounds a  = (Vertex a, Vertex a)
type Edge a    = (Vertex a, Vertex a)

graph :: (Ix a) => Bounds a -> [Edge a] -> Graph a
graph (b,e) = accumArray (flip (:)) [] (b,e)

graphU :: (Ix a) => Bounds a -> [Edge a] -> Graph a
graphU bs lst = graph bs $ nub (lst ++ map swap lst)
  where swap (a,b) = (b,a)

solve :: Int -> Graph Int -> [[Int]]
solve s g = concatMap (next [s]) (g!s)
  where
    next :: [Int] -> Int -> [[Int]]
    next v p = let v' = (p:v)
                   xs = filter (`notElem` v') (g!p)
                in case xs of
                    [] -> [reverse v']
                    _  -> concatMap (next v') xs

main :: IO ()
main = do
    [n,_] <- map read . words <$> getLine :: IO [Int]
    a <- map ((\[a,b] -> (a,b)) . map (fst . fromJust . BS.readInt) . BS.words)
        . BS.lines <$> BS.getContents :: IO [(Int,Int)]
    print . length . filter (==n) . map length . solve 1 $ graphU (1,n) a
