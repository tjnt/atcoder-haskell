main :: IO ()
main = do
    s <- getLine :: IO String
    t <- getLine :: IO String
    putStrLn $ if all (== True) $ zipWith f s t
                  then "You can win"
                  else "You will lose"
  where
    f '@' '@' = True
    f '@' b = b `elem` "atcoder"
    f a '@' = a `elem` "atcoder"
    f a b
      | a == b = True
      | otherwise = False
