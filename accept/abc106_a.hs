main = interact $ show . product . map ((-)1 . read) . words
