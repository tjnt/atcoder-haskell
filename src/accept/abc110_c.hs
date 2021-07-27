import Data.List

dupIdx :: String -> [[Int]]
dupIdx s = go $ nub s
  where
    go [] = []
    go (x:xs) = let is = elemIndices x s
                 in is : go xs

solve :: String -> String -> Bool
solve s t = dupIdx s == dupIdx t

main :: IO ()
main = do
    s <- getLine
    t <- getLine
    putStrLn $ if solve s t then "Yes" else "No"
