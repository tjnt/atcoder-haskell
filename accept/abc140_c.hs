main :: IO ()
main = do
    n <- readLn :: IO Int
    b <- map read . words <$> getLine :: IO [Int]
    print . sum . solve $ head b : b
  where
    solve [] = []
    solve (i:j:xs) = (if i > j then j else i) : solve (j:xs)
    solve (i:xs) = [i]

