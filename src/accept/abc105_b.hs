main :: IO ()
main = do
    n <- readLn :: IO Int
    let a = [ (i,j) | i <- [0..n`div`4]
                    , j <- [0..n`div`7]
                    , i * 4 + j * 7 == n ]
    putStrLn $ if null a then "No" else "Yes"
