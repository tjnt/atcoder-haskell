import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

solve :: Int -> [(Int,Int,Int)] -> V.Vector Int
solve n xs = let v = V.replicate n 0
              in V.create $ V.thaw v >>= flip go xs
  where
    go v [] = return v
    go v ((l,r,t):xs) = do
        forM_ [l..r] $ \i -> VM.write v i t
        go v xs

main :: IO ()
main = do
    [n,q] <- map read . words <$> getLine :: IO [Int]
    lrts <- map ((\[a,b,c] -> (a-1,b-1,c)) . (map read . words))
          . lines <$> getContents :: IO [(Int,Int,Int)]
    mapM_ print $ solve n lrts
