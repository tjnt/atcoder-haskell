import           Data.List
main = do
    s <- getLine
    putStrLn $ if (length . group . sort) s == 2 then "Yes" else "No"
