import           Data.List
main = do
    n <- readLn :: IO Int
    p <- map read . words <$> getLine :: IO [Int]
    print . length . filter (True==) . foldl' (\a i -> j(f i p):a) [] $ [0..(n-3)]
  where
    f n = take 3 . drop n
    j p = let [_,pi,_] = p in sort p !! 1 == pi
