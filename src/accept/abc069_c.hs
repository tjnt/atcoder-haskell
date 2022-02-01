import Data.List

main :: IO ()
main = do
    _ <- getLine
    aa <- map read . words <$> getLine :: IO [Int]
    let (n4,n2,n) = foldl' f (0,0,0) aa
        n' = n + if n2 >= 1 then 1 else 0
    putStrLn $ if n4 >= pred n' then "Yes" else "No"
  where
    f (n4,n2,n) x
      | x `mod` 4 == 0 = (succ n4,n2,n)
      | x `mod` 2 == 0 = (n4,succ n2,n)
      | otherwise = (n4,n2,succ n)
