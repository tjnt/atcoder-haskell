import Data.Array.ST
import Data.Array.IArray
import Control.Monad.ST

solve :: [Int] -> Int -> String -> Int
solve [r,s,p] k ts = let a' = runSTArray $ thaw a >>= go 1
                      in sum . map f $ elems a'
  where
    a = listArray (1,n) ts :: Array Int Char
    n = length ts
    f :: Char -> Int
    f t | t == 'r' = p
        | t == 's' = r
        | t == 'p' = s
        | otherwise = 0
    go :: Int -> STArray s Int Char -> ST s (STArray s Int Char)
    go i a
      | i > n = return a
      | otherwise = do
          x <- readArray a i
          if i <= k
             then go (i+1) a
             else do
                 y <- readArray a (i-k)
                 if x == y
                    then do writeArray a i 'x'
                            go (i+1) a
                    else go (i+1) a

main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Int]
    rsp <- map read . words <$> getLine :: IO [Int]
    t <- getLine
    print $ solve rsp k t
