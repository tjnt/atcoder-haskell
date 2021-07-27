solve :: [Int] -> Int
solve [a,b,c,x,y] =
        if a + b > 2 * c
           then (2 * c * min x y) +
               if d > 2 * c then 2 * c * z
                            else d * z
           else (a * x) + (b * y)
  where
    (z,d) = case x of _ | x < y     -> (y-x, b)
                        | x > y     -> (x-y, a)
                        | otherwise -> (0, 0)

main :: IO ()
main = print =<< solve . map read . words <$> getLine
