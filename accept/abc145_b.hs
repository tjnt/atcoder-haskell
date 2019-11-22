solve :: String -> String -> Bool
solve [] y     = False
solve (x:xs) y = if xs == reverse (x:y)
                    then True else solve xs (x:y)

main :: IO ()
main = do
    _ <- getLine
    s <- getLine :: IO String
    putStrLn $ if solve s [] then "Yes" else "No"
