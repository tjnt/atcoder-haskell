main = getLine >>= putStrLn . reverse . foldl (\a x ->
    if x == 'B' then if null a then a else tail a else x:a) []
