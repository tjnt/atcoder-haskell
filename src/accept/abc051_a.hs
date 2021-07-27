main = interact $ reverse . foldl (\a c -> if c == ',' then ' ':a else c:a) []
