solve :: String -> Int
solve s
  | s == "b" = 0
  | check s = length s `div` 2
  | otherwise = -1

check s = odd n
        && (s!!(n`div`2)) == 'b'
        && all chk (zip s (tail s))
  where
    n = length s
    chk (l,r)
      | l == 'b' = r == 'c'
      | l == 'a' = r == 'b'
      | l == 'c' = r == 'a'

main :: IO ()
main = do
    _ <- getLine
    s <- getLine
    print $ solve s
