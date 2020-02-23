import Data.List

modulus :: Integer
modulus = 10^9 + 7

fact :: Integral a => a -> a
fact n = f n 1
  where
    f 0 b = b
    f a b = b `seq` f (a - 1) (a * b)

factorization :: Integral a => a -> [a]
factorization 1 = []
factorization n = v : factorization (n `div` v)
  where
    factors x = [i | i <- [1..x], x `mod` i == 0]
    v = factors n !! 1

solve :: Integer -> Integer
solve n = foldl f 1 . group . factorization $ fact n
  where
    lengthI = fromIntegral . length
    f a g = a * (lengthI g + 1) `mod` modulus

main :: IO ()
main = do
    n <- readLn :: IO Integer
    print $ solve n
