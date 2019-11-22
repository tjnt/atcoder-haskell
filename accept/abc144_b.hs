main :: IO ()
main = do
    n <- readLn :: IO Int
    let x = [ 1 | i <- [1..9], j <- [i..9], i*j == n ]
    putStrLn $ if length x == 0 then "No" else "Yes"
