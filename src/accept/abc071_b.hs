import Data.List

main :: IO ()
main = do
    s <- getLine :: IO String
    putStrLn $ maybe "None" return $ find (`notElem` s) ['a'..'z']
