{-# LANGUAGE BangPatterns #-}

trib :: Int -> Int
trib n = f n 0 0 1
  where
    f 0 _ _ _ = 0
    f 1 _ _ c = 0
    f 2 _ _ c = c
    f !n !a !b !c = f (n-1) b c ((a+b+c) `rem` 10007)

main :: IO ()
main = readLn >>= print . trib . pred
