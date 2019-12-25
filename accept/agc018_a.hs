import qualified Data.IntMap as M

solve s t =
   if all (==True) $ M.elems xx then l else -1
  where
    n = length s
    m = length t
    l = lcm n m
    ss = M.fromList $ zip (ids n) s
    tt = M.fromList $ zip (ids m) t
    ids k = 1 : [ l `div` k * i  + 1 | i <- [1..] ]
    xx = M.intersectionWith (==) ss tt

main :: IO ()
main = do
    _ <- getLine
    [s,t] <- lines <$> getContents
    print $ solve s t
