main = getLine >>= print . (\[t,x] -> t / x) . map read . words
