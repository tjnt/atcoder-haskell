import Data.List

main :: IO ()
main = do
    w <- getLine :: IO String
    putStrLn . filter (`notElem` "aiueo") $ w
