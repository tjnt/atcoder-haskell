import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Text.Printf

solve :: [Int] -> (Int, Int)
solve xs = (r1, r2)
  where
    r1 | c07 == 0 && c8 > 0 = 1
       | otherwise = c07
    r2 = c07 + c8
    c07 = V.sum $ V.init b
    c8  = V.last b
    b = runST $ MV.replicate 9 0 >>= \v -> go v xs
    go v []     = V.freeze v
    go v (x:xs) = do
        let c = judge x
         in if c < 8
           then MV.write v c 1
           else MV.modify v succ c
        go v xs
    judge x
      | 3200 <= x = 8
      | 2800 <= x = 7
      | 2400 <= x = 6
      | 2000 <= x = 5
      | 1600 <= x = 4
      | 1200 <= x = 3
      |  800 <= x = 2
      |  400 <= x = 1
      | otherwise = 0

main :: IO ()
main = do
    _ <- getLine
    xs <- map read . words <$> getLine :: IO [Int]
    let (r1,r2) = solve xs
     in printf "%d %d" r1 r2
