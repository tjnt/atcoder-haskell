import Data.Array.IArray
import Control.Monad

solve :: Int -> Int -> [Int] -> Array (Int,Int) Int
solve h w aa = array ((0,0),(h-1,w-1)) $ zip ixs as
  where
    as = concatMap (uncurry replicate) $ zip aa [1..]
    ixs = [ if even i then (i,j) else (i,w-1-j)
          | i <- [0..h-1], j <- [0..w-1] ]

main :: IO ()
main = do
    [h,w] <- map read . words <$> getLine :: IO [Int]
    _ <- getLine
    aa <- map read . words <$> getLine :: IO [Int]
    let res = solve h w aa
    forM_ [0..h-1] $ \i -> do
        forM_ [0..w-1] $ \j -> do
            putStr . show $ res!(i,j)
            putStr " "
        putStrLn ""
