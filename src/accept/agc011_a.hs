import Data.List

solve :: Int -> Int -> [Int] -> Int
solve c k t = let (_,_,a) = foldl f (head t,1,0) (tail t) in a+1
  where
    f (y',c',a) y
      | c' == c  = (y,1,a+1)
      | y'+k < y = (y,1,a+1)
      | otherwise = (y',c'+1,a)

main :: IO ()
main = do
    [n,c,k] <- map read . words <$> getLine :: IO [Int]
    t <- sort . map read . lines <$> getContents :: IO [Int]
    print $ solve c k t
