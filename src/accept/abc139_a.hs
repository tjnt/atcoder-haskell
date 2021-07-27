main :: IO ()
main = do
    s <- getLine
    t <- getLine
    print . sum . map (\(a,b) -> if a == b then 1 else 0) $ zip s t
