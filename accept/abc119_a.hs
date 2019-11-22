main :: IO ()
main = do
    s <- getLine
    putStrLn $ if s <= "2019/04/30" then "Heisei" else "TBD"
