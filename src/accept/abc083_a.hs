main = interact $  f . map read . words
  where
    f [a, b, c, d]
        | a + b > c + d = "Left"
        | a + b < c + d = "Right"
        | otherwise     = "Balanced"

