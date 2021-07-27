main :: IO ()
main = do
    n <- getLine :: IO String
    putStrLn $ map conv n
  where
    conv c
      | c == '1'  = '9'
      | c == '9'  = '1'
      | otherwise = c
