import Control.Monad
import Data.List
import Data.Maybe

solve :: [(Int,Int)] -> [(Int,Int)] -> [Int]
solve lr = map go1
  where
    include a b x = a <= x && x <= b
    go1 (s,t) =
        let p   = fromJust $ findIndex (\(a,b) -> include a b s) lr
            lr' = drop p lr
         in p + go2 (s,t) (head lr') (tail lr') + 1
    go2 _ _ [] = 0
    go2 (s,t) (l1,r1) ((l2,r2):lr)
      | include l1 r1 s && include l1 r1 t = 0
      | otherwise = 1 + go2 (s,t) (l3,r3) lr
      where
        (l3,r3)
          | include l2 r2 l1 || include l2 r2 r1 =
              (min l1 l2, max r1 r2)
          | otherwise = (l1,r1)

main :: IO ()
main = do
    [n,d,k] <- map read . words <$> getLine :: IO [Int]
    lr <- replicateM d
        $ (\[a,b] -> (a,b)) . (map read . words)
       <$> getLine :: IO [(Int,Int)]
    st <- replicateM k
        $ (\[a,b] -> (a,b)) . (map read . words)
       <$> getLine :: IO [(Int,Int)]
    mapM_ print $ solve lr st
