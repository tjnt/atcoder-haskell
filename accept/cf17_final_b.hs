import Data.List

solve :: String -> Bool
solve [_] = True
solve [a,b] = a /= b
solve s = length g == 3 && (maximum g - minimum g) <= 1
  where
    g = map length . group . sort $ s

main :: IO ()
main = do
    s <- getLine
    putStrLn $ if solve s then "YES" else "NO"
