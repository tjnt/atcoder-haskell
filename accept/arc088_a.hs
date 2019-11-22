import Data.List

main :: IO ()
main = do
    [x,y] <- map read . words <$> getLine :: IO [Int]
    print . length $ unfoldr (f y) x
  where
    f :: Int -> Int -> Maybe (Int,Int)
    f y x
      | x > y     = Nothing
      | otherwise = Just (x, x*2)
