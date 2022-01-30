main :: IO ()
main = do
    rgb <- read . concat . words <$> getLine :: IO Int
    putStrLn $ if rgb `mod` 4 == 0 then "YES" else "NO"
