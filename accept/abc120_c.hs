main :: IO ()
main = do
    s <- getLine
    let count c = length . filter (==c)
    print $ 2 * min (count '0' s) (count '1' s)
