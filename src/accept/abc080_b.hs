solve :: Int -> Bool
solve x
  | x `mod` f x == 0 = True
  | otherwise = False
  where
    f x
      | x == 0 = 0
      | otherwise = (x `mod` 10) + f (x `div` 10)

main :: IO ()
main = do
    n <- readLn :: IO Int
    putStrLn $ if solve n then "Yes" else "No"
