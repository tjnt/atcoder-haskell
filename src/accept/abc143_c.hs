import Data.List

main :: IO ()
main = do
    n <- readLn :: IO Int
    s <- getLine :: IO String
    print . length $ group s
