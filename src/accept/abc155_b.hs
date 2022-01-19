import Data.List

f :: Int -> Bool
f n
  | even n = n `mod` 3 == 0 || n `mod` 5 == 0
  | otherwise = True

main :: IO ()
main = do
    _ <- getLine
    aa <- map read . words <$> getLine :: IO [Int]
    putStrLn $ if all f aa then "APPROVED" else "DENIED"
