import Data.List
import Data.Char

solve :: String -> Bool
solve s =  head s == 'A'
        && (length . filter (=='C')) s2 == 1
        && all isLower s'
  where s1 = take 2 s
        s2 = init . drop 2 $ s
        s3 = [last s]
        s' = tail s1 ++ delete 'C' s2 ++ s3

main :: IO ()
main = getLine >>= \s -> putStrLn $ if solve s then "AC" else "WA"
