import Data.List
import Data.Function

divisor :: Integral a => a -> [a]
divisor n = foldr f [] $ takeWhile ((<= n) . (^2)) [1..n]
  where
    f x xs
        | r == 0, q /= x = x : q : xs
        | r == 0         = x : xs
        | otherwise      = xs
      where
        (q, r) = n `divMod` x

main :: IO ()
main = do
    n <- readLn :: IO Int
    let d = sort $ divisor n
        (m,_) = minimumBy (compare `on` snd)
                [ (i, i + (n `div` i)) | i <- d]
    print $ (m-1) + (n `div` m) -1
