import Data.Array.IArray
import Data.Char

solve :: Char -> Array Char String -> Char
solve c a = case a!c of
              []     -> c
              (x:xs) -> let a' = a // [(c, xs)]
                         in solve x a'

main :: IO ()
main = do
    s <- listArray ('a', 'c') . lines <$> getContents :: IO (Array Char String)
    putChar . toUpper $ solve 'a' s
