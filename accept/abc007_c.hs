import Data.Array.IArray
import Data.Array.ST
import Control.Monad.ST
import Control.Monad

solve :: (Int,Int) -> (Int,Int) -> Array (Int,Int) Char -> Int
solve s g m = runST $ do
    a <- newArray (bounds m) 0 :: ST s (STArray s (Int,Int) Int)
    go a [s]
    readArray a g
  where
    go a [] = return ()
    go a (p:q)
      | p == g = return ()
      | otherwise = do
          ns <- filterM (readArray a >=> return . (==0)) $ next p
          case ns of
              [] -> go a q
              ns -> do
                  k <- readArray a p
                  forM_ ns $ \n -> do
                      writeArray a n (k+1)
                  go a (q ++ ns)
    next (y,x) = [ p | p <- [(y+1, x), (y, x+1), (y-1, x), (y, x-1)], m!p /= '#' ]

main :: IO ()
main = do
    [r,c] <- map read . words <$> getLine :: IO [Int]
    [sy,sx] <- map read . words <$> getLine :: IO [Int]
    [gy,gx] <- map read . words <$> getLine :: IO [Int]
    cc <- concat . lines <$> getContents :: IO String
    let m = listArray ((1,1),(r,c)) cc :: Array (Int,Int) Char
    print $ solve (sy,sx) (gy,gx) m
