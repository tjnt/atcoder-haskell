import Data.Bits

solve :: [[Int]] -> Bool
solve ttt = elem 0 $ foldl1 f ttt
  where
    f l r = [ x `xor` y | x <- l, y <- r ]
    
main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Int]
    ttt <- map (map read . words) . lines <$> getContents :: IO [[Int]]
    putStrLn $ if solve ttt then "Found" else "Nothing"
