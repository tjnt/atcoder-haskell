main = getLine >>= print . length .
    dropWhile (/='Z') . reverse . dropWhile (/='A')
