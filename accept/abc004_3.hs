import Data.Array.IArray
import Data.Array.ST
import Control.Monad.ST

solve :: Int -> Array Int Int
solve n =
    let ary = listArray (1,6) [1..6] :: Array Int Int
     in runSTArray $ thaw ary >>= go 0
  where
    go :: Int -> STArray s Int Int -> ST s (STArray s Int Int)
    go i a 
      | i == n = return a
      | otherwise =
          let x = i `mod` 5 + 1
           in do l <- readArray a x
                 r <- readArray a (x+1)
                 writeArray a x r
                 writeArray a (x+1) l
                 go (i+1) a

main :: IO ()
main = do
    n <- readLn :: IO Int
    putStrLn . concatMap show . elems $ solve (n `mod` 30)
