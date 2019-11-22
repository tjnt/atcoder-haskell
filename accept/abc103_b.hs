import Data.List
main :: IO ()
main = do
    s <- getLine :: IO String
    t <- getLine :: IO String
    putStrLn $ if isInfixOf t (s ++ s) then "Yes" else "No"
