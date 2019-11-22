import           Control.Monad

comb :: Int -> [a] -> [[a]]
comb 0 xs     = [[]]
comb _ []     = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] ++ comb n xs

norm :: (Integral a, Floating b) => [a] -> [a] -> b
norm i j = sqrt . fromIntegral . sum $ zipWith (\a b -> (a - b) ^ 2) i j

isInteger :: RealFrac a => a -> Bool
isInteger x = x == fromInteger (round x)

main = do
    [n,_] <- map read . words <$> getLine :: IO [Int]
    x <- replicateM n (map read . words <$> getLine) :: IO [[Int]]
    print $ length . filter isInteger . map (\[i, j] -> norm i j) $ comb 2 x
