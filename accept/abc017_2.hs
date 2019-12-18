import Data.List

solve :: String -> Bool
solve [] = True
solve [c] = c `elem` "oku"
solve (a:b:xs)
  | [a,b] == "ch" = solve xs
  | a `elem` "oku" = solve (b:xs)
  | otherwise = False

main :: IO ()
main = do
    x <- getLine
    putStrLn $
        if solve x then "YES" else "NO"
