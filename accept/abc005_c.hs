solve :: Int -> [Int] -> [Int] -> Bool
solve t _ [] = True
solve t [] _ = False
solve t (x:xs) (y:ys)
  | x <= y &&  y <= x+t = solve t xs ys
  | otherwise  = solve t xs (y:ys)

main :: IO ()
main = do
    t <- readLn :: IO Int
    n <- readLn :: IO Int
    a <- map read . words <$> getLine :: IO [Int]
    m <- readLn :: IO Int
    b <- map read . words <$> getLine :: IO [Int]
    putStrLn $ if solve t a b then "yes" else "no"
