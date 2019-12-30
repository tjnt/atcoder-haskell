import Data.Maybe
import Data.Array.IArray
import Data.Array.ST
import Control.Monad.ST

solve :: Array (Int,Int) Char -> Bool
solve a = notElem '#' . elems $ runSTArray $ thaw a >>= go (1,1)
  where
    (_,(bh,bw)) = bounds a
    cand (h,w) = [ p | p@(ph,pw) <- [(h+1,w), (h,w+1)]
                 , ph <= bh, pw <= bw, a!p == '#' ]
    next p = case cand p of
               []  -> Nothing
               [x] -> Just x
               _   -> Nothing
    go :: (Int,Int) -> STArray s (Int,Int) Char
          -> ST s (STArray s (Int,Int) Char)
    go p m = do
        writeArray m p '.'
        maybe (return m) (`go` m) $ next p

main :: IO ()
main = do
    [h,w] <- map read . words <$> getLine :: IO [Int]
    a <- listArray ((1,1),(h,w)) . concat . lines
         <$> getContents :: IO (Array (Int,Int) Char)
    putStrLn $ if solve a then "Possible" else "Impossible"
