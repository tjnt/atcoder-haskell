import Data.List

tpl [a,b] = (a,b)

main :: IO ()
main = do
    s <- getLine :: IO String
    let ix = tpl . take 2 . elemIndices ('W','W') $ zip s (tail s)
    putStrLn $ case ix of
                   (4,11) -> "Do"
                   (2, 9) -> "Re"
                   (0, 7) -> "Mi"
                   (6,11) -> "Fa"
                   (4, 9) -> "So"
                   (2, 7) -> "La"
                   (0, 5) -> "Si"
