import Data.List
import Data.Ord

main :: IO ()
main = do
    (n:p)  <- map read . lines <$> getContents :: IO [Int]
    let p1:ps = sortOn Down p
    print $ (p1 `div` 2) + sum ps
