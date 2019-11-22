import Control.Monad
import Data.Array.IArray

main :: IO ()
main = do
    [n,t] <- map read . words <$> getLine :: IO [Int]
    ts <- map read . words <$> getLine :: IO [Int]
    let ta = listArray (0,n-1) ts :: Array Int Int
    print . (+t) . sum . map (\i -> min ((ta!i) - (ta!(i-1))) t) $ [1..n-1]
