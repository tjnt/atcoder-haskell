import           Data.List

f :: (Bool, Char) -> Char -> (Bool, Char)
f (a, b) c = (a || (b == c), c)

main = do
    s <- getLine
    putStrLn $ if (fst . foldl' f (False, ' ')) s
                  then "Bad" else "Good"
