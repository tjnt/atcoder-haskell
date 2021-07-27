import Data.List
import Data.Ord

main :: IO ()
main = do
    s <- getLine :: IO String
    t <- getLine :: IO String
    let s' = sort s
        t' = sortOn Down t
    putStrLn $ if s' < t' then "Yes" else "No"
