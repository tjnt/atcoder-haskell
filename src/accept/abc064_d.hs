import qualified Data.Sequence as Q
import Data.Sequence
import Data.Foldable

solve :: String -> Seq Char
solve = go 0 empty
  where
    go i s [] = s >< Q.replicate i ')'
    go i s ('(':xs) = go (succ i) (s |> '(') xs
    go i s (')':xs)
      | i > 0 = go (pred i) (s |> ')') xs
      | otherwise = go i (('(' <| s) |> ')') xs
    go _ _ (_:_) = undefined

main :: IO ()
main = do
    _ <- getLine
    s <- getLine :: IO String
    putStrLn . toList $ solve s
