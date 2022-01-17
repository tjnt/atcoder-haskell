import qualified Data.Vector as V

solve :: Int -> [Int] -> Int
solve k aa = go 0
  where
    v = V.scanl (+) 0 $ V.fromList aa
    go :: Int -> Int
    go i
      | i + k < V.length v =
          let l = v V.! i
              r = v V.! (i+k)
           in r-l + go (i+1)
      | otherwise = 0

main :: IO ()
main = do
    [_,k] <- map read . words <$> getLine :: IO [Int]
    aa <- map read . words <$> getLine :: IO [Int]
    print $ solve k aa
