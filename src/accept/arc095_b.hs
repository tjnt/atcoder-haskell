import Data.List
import Data.Function

ceilDiv :: Integral a => a -> a -> a
ceilDiv t s = (t + s - 1) `div` s

solve :: [Int] -> [Int]
solve a = [n,r]
  where
    sa = sort a
    n  = last sa
    r  = let f i = abs (n `ceilDiv` 2 - i)
          in minimumBy (compare `on` f) $ init sa

main :: IO ()
main = do
    _ <- getLine
    a <- map read . words <$> getLine :: IO [Int]
    mapM_ print $ solve a
