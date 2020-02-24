import qualified Data.IntMap as M
import Data.Array.IArray

solve :: Int -> [Int] -> [Int]
solve n a = go n M.empty
  where
    a' = listArray (1,n) a :: Array Int Int
    go :: Int -> M.IntMap Bool -> [Int]
    go 0 m = [ k | (k,v) <- M.toAscList m, v ]
    go i m = let ks = [ j | j <- [i,i*2..n], j `mod` i == 0 ]
                 l = length $ filter (\k ->
                        maybe False id (M.lookup k m)) ks
                 b = if l `mod` 2 == (a'!i) then False else True
                 m' = M.insert i b m
              in go (i-1) m'

main :: IO ()
main = do
    n <- readLn :: IO Int
    a <- map read . words <$> getLine :: IO [Int]
    let ret = solve n a
    print $ length ret
    mapM_ print ret
