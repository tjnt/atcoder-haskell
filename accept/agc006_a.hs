f _ [] = 0
f [] _ = 0
f (x:xs) (y:ys) =
    if x == y then 1 + f xs ys else 0

solve :: String -> String -> Int
solve _ [] = 0
solve [] _ = 0
solve s t = max (f s t) (solve (tail s) t)

main :: IO ()
main = do
    n <- readLn :: IO Int
    s <- getLine :: IO String
    t <- getLine :: IO String
    print $ 2 * n - solve s t
