main = getLine >>= print . (\[a,b] ->
    if b `mod` a == 0 then a + b else b - a) . map read . words
