solve s t = all (==True) $ zipWith f s t
  where
    f l r
      | l == r = True
      | l == '@' = r `elem` "atcoder"
      | r == '@' = l `elem` "atcoder"
      | otherwise = False

main :: IO ()
main = do
    s <- getLine
    t <- getLine
    putStrLn $ if solve s t then "You can win" else "You will lose"
