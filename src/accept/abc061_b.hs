import qualified Data.IntMap as M

countMap :: [Int] -> M.IntMap Int
countMap xs = M.fromListWith (+) $ zip xs (repeat 1)

main :: IO ()
main = do
    [n,m] <- map read . words <$> getLine :: IO [Int]
    xs <- map (map read . words) . lines <$> getContents :: IO [[Int]]
    let m = countMap $ concat xs
     in mapM_ (print . \i -> M.findWithDefault 0 i m) [1..n]
