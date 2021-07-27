main :: IO ()
main = do
    s <- getLine
    putStrLn $ if all f (zip [1..] s) then "Yes" else "No"
  where
    f (i,c) = let xx = if odd i then "RUD" else "LUD"
             in elem c xx
