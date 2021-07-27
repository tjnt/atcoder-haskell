import Data.List
import Data.Ord

main :: IO ()
main = do
    x <- map read . words <$> getLine :: IO [Int]
    let [a,b,c] = sortOn Data.Ord.Down x
    print $ a * 10 + b + c
