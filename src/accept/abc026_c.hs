import Data.Array.IArray

type Vertex a  = a
type Graph a   = Array (Vertex a) [Vertex a]
type Bounds a  = (Vertex a, Vertex a)
type Edge a    = (Vertex a, Vertex a)

graph :: (Ix a) => Bounds a -> [Edge a] -> Graph a
graph (b,e) = accumArray (flip (:)) [] (b,e)

solve :: [Int] -> Int
solve b = go 1
  where
    n = length b + 1
    g = graph (1,n) $ zip b [2..]
    go i =
      case g!i of
        []  -> 1
        [x] -> 2 * go x + 1
        xs  -> let m = map go xs
                in maximum m + minimum m + 1

main :: IO ()
main = do
    _ <- getLine
    b <- map read . lines <$> getContents :: IO [Int]
    print $ solve b
