main = interact $ show . (\[a, b] ->
    if a <= 5 then 0
    else if 6 <= a && a <= 12 then b `div` 2
    else b) . map read . words
