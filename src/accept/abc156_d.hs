import Data.List
import Data.Int

modulus :: Int64
modulus = 10^9 + 7

addMod, subMod, mulMod, remMod :: Int64 -> Int64 -> Int64
addMod x y
  | x + y >= modulus = x + y - modulus
  | otherwise        = x + y

subMod x y
  | x < y     = x - y + modulus
  | otherwise = x - y

mulMod x y = (x * y) `rem` modulus

remMod x y = x `mulMod` invMod y

powMod :: Int64 -> Int64 -> Int64
powMod n p
  | p == 0    = 1
  | odd p     = n * powMod n (p-1) `mod` modulus
  | otherwise = let t = powMod n (p `div` 2)
                 in (t^2) `mod` modulus

invMod :: Int64 -> Int64
invMod n = powMod n (modulus - 2)

prodMod :: [Int64] -> Int64
prodMod = foldr mulMod 1

nCrMod :: Int64 -> Int64 -> Int64
nCrMod n r = prodMod [(n-r+1)..n] `remMod` prodMod [1..r]

main :: IO ()
main = do
    [n,a,b] <- map read . words <$> getLine :: IO [Int64]
    print $ (powMod 2 n - 1) `subMod` nCrMod n a `subMod` nCrMod n b
