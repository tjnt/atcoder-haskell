import Data.Char

concatIntList :: Integral a => [a] -> a
concatIntList xs = go (length xs - 1) xs 0
  where
    go _ [] sum     = sum
    go n (x:xs) sum = go (n-1) xs (sum + x * 10^n)

minusIntList :: [Int] -> [Int] -> Int
minusIntList a b = concatIntList a - concatIntList b

solve :: [Int] -> [Int] -> [Int]
solve [a1,a2,a3] [b1,b2,b3] =
            [ minusIntList [9,a2,a3]  [b1,b2,b3]
            , minusIntList [a1,9,a3]  [b1,b2,b3]
            , minusIntList [a1,a2,9]  [b1,b2,b3]
            , minusIntList [a1,a2,a3] [1,b2,b3]
            , minusIntList [a1,a2,a3] [b1,0,b3]
            , minusIntList [a1,a2,a3] [b1,b2,0]
            , minusIntList [a1,a2,a3] [b1,b2,b3]
            ]

main :: IO ()
main = do
    [a,b] <- map (map digitToInt) . words <$> getLine :: IO [[Int]]
    print . maximum $ solve a b
