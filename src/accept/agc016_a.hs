import Data.List
import Data.Function

solve :: String -> Char -> Int
solve []  _ = 0
solve s c
  | all (==c) s = 0
  | otherwise   = 1 + solve (go s) c
  where
    go []  = []
    go [_] = []
    go (s1:s2:ss) = (if s1 == c || s2 == c
                        then c else s1) : go (s2:ss)

main :: IO ()
main = do
    s <- getLine :: IO String
    print . minimum $ map (solve s) (nub s)
