import           Data.Array.IArray
import           Data.List

type Graph = Array Int [Int]

solve n xs = [(length . go) i | i <- [1..n]]
  where
    xs' = xs ++ map swap xs
    swap (x,y) = (y,x)
    g = accumArray (flip (:)) [] (1,n) xs' :: Graph
    go i = nub [k | j <- g!i, k <- g!j, i /= k] \\ g!i

main :: IO ()
main = do
    [n,_] <- map read . words <$> getLine :: IO [Int]
    xs <- map ((\[a,b] -> (a,b)) . (map read . words))
        . lines <$> getContents :: IO [(Int,Int)]
    mapM_ print $ solve n xs
