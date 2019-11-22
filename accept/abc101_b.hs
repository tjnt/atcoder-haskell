digitSum :: Integral a => a -> a
digitSum 0 = 0
digitSum n = n `mod` 10 + digitSum (n `div` 10)

main :: IO ()
main = do
    n <- readLn :: IO Int
    putStrLn $ if n `rem` digitSum n == 0 then "Yes" else "No"
