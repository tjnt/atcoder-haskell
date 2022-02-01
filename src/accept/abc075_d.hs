import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.Graph

buildGU :: Bounds -> [Edge] -> Graph
buildGU b xs = buildG b xs'
  where
    xs' = xs ++ map swap xs
    swap (x,y) = (y,x)

isLinkedAll :: Graph -> [Int] -> Bool
isLinkedAll g vs = all go vs
  where
    n = length vs
    go v = length (reachable g v) == n

solve :: Int -> [Edge] -> Int
solve n es = length . filter (==False)
           $ map (`isLinkedAll` vs) gs
  where
    vs = [1..n]
    gs = [ buildGU (1,n) (filter (/=e) es) | e <- es ]

main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    abs <- map ((\[a,b] -> (a,b)) . map (fst . fromJust . BS.readInt) . BS.words)
        . BS.lines <$> BS.getContents :: IO [(Int,Int)]
    print $ solve n abs
