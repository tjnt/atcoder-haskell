import Data.List
main = getLine >>= print . foldl (\a c -> if c == '+' then a+1 else a-1) 0
