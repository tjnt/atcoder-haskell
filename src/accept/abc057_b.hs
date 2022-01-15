import Control.Monad
import Data.List
import Data.Ord (comparing)

solve :: [(Int,Int)] -> [(Int,Int)] -> [Int]
solve xs ys = map go xs
  where
    go (a,b) = let d = [ dist a b c d | (c,d) <- ys ]
                in fst . minimumBy (comparing snd) $ zip [1..] d
    dist x1 y1 x2 y2 = abs (x1 - x2) + abs (y1 - y2)

main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    abs <- replicateM n $ (\[a,b] -> (a,b)) . (map read . words)
                       <$> getLine :: IO [(Int,Int)]
    cds <- replicateM m $ (\[a,b] -> (a,b)) . (map read . words)
                       <$> getLine :: IO [(Int,Int)]
    mapM_ print $ solve abs cds
