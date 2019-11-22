import Control.Monad
import Data.Array.IArray

sa :: Int -> Int -> Int -> Int
sa a1 a2 a3 = abs (a2 - a1) + abs (a3 - a2) - abs (a3 - a1)

main :: IO ()
main = do
    n <- readLn :: IO Int
    a <- map read . words <$> getLine :: IO [Int]
    let xx = sum $ zipWith (\x y -> abs (y-x)) (0:a) (a++[0])
        aa = listArray (0,n+1) ((0:a) ++ [0]) :: Array Int Int
    forM_ [1..n] $ \i -> do
        if (aa!(i-1) <= aa!i && aa!i > aa!(i+1)) ||
           (aa!(i-1) >= aa!i && aa!i < aa!(i+1))
           then print $ xx - (sa (aa!(i-1)) (aa!i) (aa!(i+1)))
           else print xx
