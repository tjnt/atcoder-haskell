main :: IO ()
main = do
    [a,b,c] <- map read . words <$> getLine :: IO [Int]
    let  p = a + b
         m = a - b
    putStrLn $ case c of
        c | c == p && c == m -> "?"
          | c /= p && c /=m -> "!"
          | c == p -> "+"
          | c == m -> "-"
    return ()
