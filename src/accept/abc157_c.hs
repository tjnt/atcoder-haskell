import qualified Data.IntMap as M
import Data.Maybe
import Data.List

import Debug.Trace
_t x = traceShow x x

concatIntList :: Integral a => [a] -> a
concatIntList xs = go (length xs - 1) xs 0
  where
    go _ [] sum     = sum
    go n (x:xs) sum = go (n-1) xs (sum + x * 10^n)

solve :: Int -> [(Int,Int)] -> Int
solve n sc = maybe (-1) go2 $ go sc M.empty
  where
    go [] m = Just m
    go ((i,v):xs) m
      | n >= 2 && i == 1 && v == 0 = Nothing
      | otherwise =
          case M.lookup i m of
              Nothing -> go xs (M.insert i v m)
              Just vv -> if vv == v
                           then go xs (M.insert i v m) else Nothing
    go2 m = let l = map (\i -> fromMaybe 0 (M.lookup i m)) [1..n]
             in rep0 l
    rep0 xs = let (s1,s2) = span (==0) xs
                  ss = if n == 1
                          then [0]
                          else (1 : replicate (length s1 - 1) 0) ++ s2
               in case s1 of
                   [] -> concatIntList xs
                   otherwise -> concatIntList ss

main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    sc <- map ((\[a,b] -> (a,b)) . (map read . words))
        . lines <$> getContents :: IO [(Int,Int)]
    print $ solve n sc
