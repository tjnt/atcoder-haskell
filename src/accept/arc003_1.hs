solve :: String -> Double
solve xs = let s = sum $ map f xs
            in s / n
  where
    n = fromIntegral $ length xs :: Double
    f :: Char -> Double
    f 'A' = 4.0
    f 'B' = 3.0
    f 'C' = 2.0
    f 'D' = 1.0
    f 'F' = 0.0

main :: IO ()
main = do
    _ <- getLine
    r <- getLine
    print $ solve r
