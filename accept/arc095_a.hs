import Data.List
import Data.Array.IArray
import Control.Monad

main :: IO ()
main = do
    n <- readLn :: IO Int
    x <- map read . words <$> getLine :: IO [Int]
    let x' = listArray (0,n-1) $ sort x :: Array Int Int
        m1 = n `div` 2 - 1
        m2 = m1 + 1
    forM_ x $ \i -> print $ if i >= x'!m2 then x'!m1 else x'!m2
