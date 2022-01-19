import qualified Data.Set as S

solve :: [Int] -> Int
solve xs = S.size . S.fromList $ map div2 xs
  where
    div2 n
      | even n = div2 (n `quot` 2)
      | otherwise = n

main :: IO ()
main = do
    _ <- getLine
    aa <- map read . words <$> getLine :: IO [Int]
    print $ solve aa
