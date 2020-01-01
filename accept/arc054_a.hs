solve :: [Double] -> Double
solve [l,x,y,s,d]
  | s == d    = 0
  | y < x     = mvl
  | otherwise = min mvl mvr
  where
    mvl | s < d = (d - s) / (x + y)
        | s > d = (l - s + d) / (x + y)
    mvr | s < d = (s + l - d) / (y - x)
        | s > d = (s - d) / (y - x) 

main :: IO ()
main = getLine >>= print . solve . map read . words
