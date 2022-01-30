solve :: String -> Int
solve [] = 0
solve ss
  | f = n
  | otherwise = solve $ init ss
  where
    n = length ss
    f | even n = let (s1,s2) = splitAt (n`div`2) ss
                  in s1 == s2 
      | otherwise = False

main :: IO ()
main = do
    s <- getLine :: IO String
    print . solve $ init s
