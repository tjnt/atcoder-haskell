main = do
    b <- getChar
    putChar $ case b of
        'A' -> 'T'
        'C' -> 'G'
        'T' -> 'A'
        'G' -> 'C'
