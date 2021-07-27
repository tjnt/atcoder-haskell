import Data.List

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
          | otherwise = True

divisor :: Int -> [Int]
divisor n = foldr f [] $ takeWhile ((<= n) . (^ 2)) [1 .. n]
  where
    f x ds
        | r == 0, q /= x = x : q : ds
        | r == 0         = x : ds
        | otherwise      = ds
      where
        (q, r) = n `divMod` x

solve :: [Int] -> [Int]
solve [] = []
solve (x:xs) = x : solve (filter (\i -> (i `mod` x) /= 0) xs)

main :: IO ()
main = do
    [a,b] <- map read . words <$> getLine :: IO [Int]
    let g = gcd a b
        (_:gs) = sort $ divisor g
    print . (+1) . length $ solve gs
