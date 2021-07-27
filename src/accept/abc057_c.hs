divisor :: Integral a => a -> [a]
divisor n = foldr f [] $ takeWhile ((<= n) . (^2)) [1..n]
  where
    f x xs
        | r == 0, q /= x = x : q : xs
        | r == 0         = x : xs
        | otherwise      = xs
      where
        (q, r) = n `divMod` x

digitNum :: Integral a => a -> a
digitNum = go 0
  where
    go i 0 = i
    go i x = go (i+1) (x `div` 10)

solve :: Int -> Int
solve n = f a b
  where
    d     = divisor n
    (a,b) = let a = last d
                b = n `div` a
             in (a,b)
    f a b = max (digitNum a) (digitNum b)

main :: IO ()
main = do
    n <- readLn :: IO Int
    print $ solve n
