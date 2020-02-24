import Data.List

modulus :: Int
modulus = 10^9 + 7

powMod :: Int -> Int -> Int
powMod n p
  | p == 0    = 1
  | odd p     = n * powMod n (p-1) `mod` modulus
  | otherwise = let t = powMod n (p `div` 2)
                 in (t^2) `mod` modulus

solve :: Int -> [Int] -> Int
solve n a
  | even n = if chk e then powMod 2 (n `div` 2) else 0
  | odd  n = if chk o then powMod 2 ((n-1) `div` 2) else 0
  where
    e = concat [ [i,i] | i <- [1,3..]]
    o = 0 : concat [ [i,i] | i <- [2,4..]]
    chk ex = all (uncurry (==)) . zip ex $ sort a

main :: IO ()
main = do
    n <- readLn :: IO Int
    a <- map read . words <$> getLine :: IO [Int]
    print $ solve n a
