import           Data.List
main = do
    [r,d,x] <- map read . words <$> getLine
    mapM_ print . tail . foldl' (\xi i -> xi ++ [r * (xi !! (i - 1)) - d]) [x] $ [1..10]
