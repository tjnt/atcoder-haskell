solve :: Int -> Int
solve x = (d * 2) + r
  where
    (d,m) = x `divMod` 11
    r = case m of
          _ | m == 0 -> 0
            | m <= 6 -> 1
            | m >  6 -> 2

main :: IO ()
main = do
    x <- readLn :: IO Int
    print $ solve x
