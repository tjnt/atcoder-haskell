main = getLine >>= print . (\[a, p] -> (a * 3 + p) `div` 2) . map read . words
