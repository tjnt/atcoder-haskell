import Data.Array.IArray

f :: Char -> (Int, [(Int,Int)]) -> (Int,Char) -> (Int, [(Int,Int)])
f c (a, r) (i,x) =
    if c == x then (a+1, r) else (0, (i, a):r) 

ceilDiv :: Integral a => a -> a -> a
ceilDiv t s = (t + s - 1) `div` s

solve :: String -> [Int]
solve s = zipWith (+) (elems ar) (elems al)
  where
    n = length s
    rs = snd . foldl (f 'R') (0,[]) $ zip [0..] s
    ls = snd . foldr (flip (f 'L')) (0,[]) $ zip [0..] s
    rs' = concatMap (\(i,v) -> [(i-1,v `ceilDiv` 2),(i,v `div` 2)]) rs
    ls' = concatMap (\(i,v) -> [(i,v `div` 2),(i+1,v `ceilDiv` 2)]) ls
    ar = accumArray (+) 0 (0,n-1) rs' :: Array Int Int
    al = accumArray (+) 0 (0,n-1) ls' :: Array Int Int

main :: IO ()
main = getLine >>= mapM_ print . solve
