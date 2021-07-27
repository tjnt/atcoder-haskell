solve :: (Int,Int) -> (Int,Int) -> Int -> Int -> [(Int,Int)] -> Bool
solve _ _ _ _ [] = False
solve s g t v (x:xs)
  | let d  = calc s x + calc x g
        tv = fromIntegral $ t * v :: Double
     in d <= tv = True
  | otherwise = solve s g t v xs
  where
    calc (ax,ay) (bx,by) = sqrt . fromIntegral $
                             (bx - ax) ^ 2 + (by - ay) ^ 2

main :: IO ()
main = do
    [sx, sy, gx, gy, t, v] <- map read . words <$> getLine :: IO [Int]
    n <- readLn :: IO Int
    xs <- map ((\[a,b] -> (a,b)) . (map read . words))
        . lines <$> getContents :: IO [(Int,Int)]
    putStrLn $ if solve (sx,sy) (gx,gy) t v xs
                  then "YES" else "NO"
