import Control.Monad
import Data.Array.IArray

solve :: Int -> Int -> Array (Int,Int) Char -> Array (Int,Int) Char -> Bool
solve n m a b = go (1,1)
  where
    go ix@(i,j)
      | all (check ix) (range ((1,1),(m,m))) = True
      | otherwise = maybe False go (next ix)
    check (i,j) bix@(bi,bj) =
      let aix = (i+bi-1,j+bj-1)
       in (a!aix) == (b!bix)
    next (i,j)
      | i == (n-m+1) = Just (1,j+1)
      | j == (n-m+1) = Nothing
      | otherwise = Just (i+1,j)

main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    a <- listArray ((1,1),(n,n)) . concat
      <$> replicateM n getLine :: IO (Array (Int,Int) Char)
    b <- listArray ((1,1),(m,m)) . concat
      <$> replicateM m getLine :: IO (Array (Int,Int) Char)
    putStrLn $ if solve n m a b then "Yes" else "No"
