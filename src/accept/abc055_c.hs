modulus :: Int
modulus = 10^9 + 7

mulMod :: Int -> Int -> Int
mulMod x y = (x * y) `mod` modulus

main :: IO ()
main = do
    n <- readLn :: IO Int
    print $ foldl1 mulMod [1..n]
