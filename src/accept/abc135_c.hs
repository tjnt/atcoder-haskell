main = do
    n <- readLn :: IO Int
    a <- map read . words <$> getLine :: IO [Int]
    b <- map read . words <$> getLine :: IO [Int]
    print $ f a b
  where
    f _ [] = 0
    f (ai:a) (bi:b) =
        let m1 = min bi ai
            (aj:aa) = a
            m2 = min (bi - m1) aj
        in m1 + m2 + f (aj - m2:aa) b
