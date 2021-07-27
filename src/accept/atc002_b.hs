powMod :: Integral a => a -> a -> a -> a
powMod n p m
  | p == 0    = 1
  | odd p     = n * powMod n (p-1) m `mod` m
  | otherwise = let t = powMod n (p `div` 2) m
                 in (t^2) `mod` m

main :: IO ()
main = do
    [n,m,p] <- map read . words <$> getLine :: IO [Int]
    print $ powMod n p m
