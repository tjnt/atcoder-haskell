diff :: Integral a => a -> a -> a
diff n x = (n - x `mod` n) `mod` n

solve :: [(Int,Int)] -> Int
solve = foldr f 0
  where
     f (a,b) acc = acc + diff b (acc + a)

main :: IO ()
main = do
    _ <- getLine
    a <- map ((\[a,b] -> (a,b)) . (map read . words))
        . lines <$> getContents :: IO [(Int,Int)]
    print $ solve a
