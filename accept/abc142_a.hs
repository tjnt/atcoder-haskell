main :: IO ()
main = do
    n <- readLn :: IO Int
    let n' = fromIntegral n :: Double
        a = fromIntegral (if odd n then n `div` 2 + 1 else n `div` 2) :: Double
    print $ a / n'
