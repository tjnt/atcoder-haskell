main = do
    [n,u] <- map read . words <$> getLine :: IO [Int]
    putStrLn . unwords . map show . head $
        [ [x, y, z] |
          x <- [0..n],
          y <- [0..n-x],
          let z = n-x-y,
          10000 * x + 5000 * y + 1000 * z == u
        ] ++ [replicate 3 (-1)]
