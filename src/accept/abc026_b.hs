import Data.List

solve :: [Double] -> Double
solve xs = go1 0 xs * pi
  where
    go1 r [] = r
    go1 r (x:xs) = go2 (r + (x^2)) xs
    go2 r [] = r
    go2 r (x:xs) = go1 (r - (x^2)) xs

main :: IO ()
main = do
    _ <- getLine
    rr <- map read . lines <$> getContents :: IO [Double]
    let xs = reverse . sort $ rr
    print $ solve xs
