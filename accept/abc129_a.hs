main = interact $ show . minimum . (\[p, q, r] -> [p+q, p+r, q+r]) . map read . words
