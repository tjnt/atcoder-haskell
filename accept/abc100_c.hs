divableN :: Integral a => a -> a -> a
divableN n x
  | x `rem` n /= 0 = 0
  | otherwise      = 1 + divableN n (x `div` n)

main :: IO ()
main = do
    n <- readLn :: IO Int
    a <- map read . words <$> getLine :: IO [Int]
    print . sum . map (divableN 2) $ a
