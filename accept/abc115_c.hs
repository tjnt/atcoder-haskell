import Data.List
import Data.Array.IArray

solve h k i = h!(i+k-1) - h!i 

main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Int]
    h <- map read . lines <$> getContents :: IO [Int]
    let h' = listArray (0,n-1) (sort h) :: Array Int Int
    print . minimum $ map (solve h' k) [0..n-k]
