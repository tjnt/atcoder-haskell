solve :: Int -> Int
solve h = go h
  where
    go 1 = 1
    go h = 2 * go (h `div` 2) + 1

main :: IO ()
main = do
    h <- readLn :: IO Int
    print $ solve h
