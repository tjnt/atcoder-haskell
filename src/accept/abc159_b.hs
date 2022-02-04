solve :: String -> Bool
solve s = b1 && b2 && b3
  where
    n = length s
    b1 = s == reverse s
    b2 = let s' = take ((n-1)`div`2) s
          in s' == reverse s'
    b3 = let s' = drop ((n+3)`div`2-1) s
          in s' == reverse s'

main :: IO ()
main = do
    s <- getLine :: IO String
    putStrLn $ if solve s then "Yes" else "No"
