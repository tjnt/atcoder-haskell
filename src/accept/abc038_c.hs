solve :: [Int] -> Int
solve xs = let (r,_,_) = foldl f (1,1,head xs) $ tail xs
            in r
  where
    f (r,c,x1) x2 =
        let c' = if x1 < x2 then c+1 else 1
         in  (r+c', c', x2)

main :: IO ()
main = do
    _ <- getLine
    aa <- map read . words <$> getLine :: IO [Int]
    print $ solve aa
