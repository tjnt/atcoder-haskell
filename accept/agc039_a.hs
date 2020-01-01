import Data.List

solve :: String -> Int -> Int
solve s k
  | all (== head s) s = length s * k `div` 2
  | head s /= last s  = (* k) $ f g
  | otherwise = f [h] + f [h++l] * (k-1) + f m * k + f [l]
  where
    g = group s
    h = head g
    l = last g
    m = (init . tail) g
    f = sum . map ((`div` 2) . length)

main :: IO ()
main = do
    s <- getLine
    k <- readLn :: IO Int
    print $ solve s k
