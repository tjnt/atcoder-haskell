main :: IO ()
main = do
    x <- head <$> getLine :: IO Char
    let xs = zip ['A'..'E'] [1..]
    print . head $ [ i | (a,i) <- xs, a == x]
