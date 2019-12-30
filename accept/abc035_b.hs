solve :: String -> Int -> Int
solve s t = if t == 1
               then abs x + abs y + k
               else max (n `mod` 2) ((abs x + abs y) - k)
  where
    n = length s
    k = length . filter (=='?') $ s
    p@(x,y) = foldl f (0,0) s
    f (x,y) 'L' = (x-1,y)
    f (x,y) 'R' = (x+1,y)
    f (x,y) 'U' = (x,y+1)
    f (x,y) 'D' = (x,y-1)
    f (x,y) '?' = (x,y)

main :: IO ()
main = do
    s <- getLine
    t <- readLn :: IO Int
    print $ solve s t
