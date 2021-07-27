import Data.List

solve :: String -> String
solve = concatMap f . group
  where
    f z = head z : show (length z)

main :: IO ()
main = do
    s <- getLine
    putStrLn $ solve s
