main = interact $ judge . conv . map read . words
  where
    conv = map (\n -> if n == 1 then 100 else n)
    judge [a, b]
      | a > b = "Alice"
      | a < b = "Bob"
      | a ==b = "Draw"
