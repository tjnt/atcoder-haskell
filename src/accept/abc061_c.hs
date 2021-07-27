import Data.List

solve :: Int -> [(Int,Int)] -> Int
solve k ab = fst $ so!!i
  where
    i  = fst . last . takeWhile (\t -> snd t < k) $ zip [0..] sl
    sl = scanl (\a t -> a + snd t) 0 so
    so = sortOn fst ab

main :: IO ()
main = do
    [n,k] <- map read . words <$> getLine :: IO [Int]
    ab <- map ((\[a,b] -> (a,b)) . (map read . words))
        . lines <$> getContents :: IO [(Int,Int)]
    print $ solve k ab
