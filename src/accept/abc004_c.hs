import Data.Array.IO
import Control.Monad

main :: IO ()
main = do
    n <- readLn :: IO Int
    let (d,m) = let (x,m) = n `divMod` 5
                 in (x `mod` 6, m)
    let xs = let l = [1..6]
              in drop d l ++ take d l
    a <- newListArray (0,5) xs :: IO (IOArray Int Int)
    forM_ [0..m-1] $ \i -> do
        l <- readArray a i
        r <- readArray a (i+1)
        writeArray a i r
        writeArray a (i+1) l
    getElems a >>= putStrLn . concatMap show
