import Data.Char

main :: IO ()
main = do
    (x:xs) <- getLine :: IO String
    putStrLn $ toUpper x : map toLower xs
