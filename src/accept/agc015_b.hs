import Data.Array.IArray

f :: Array Int Char -> Int -> Int -> Int
f a n i
  | (a!i) == 'U' = i * 2 + n - i
  | (a!i) == 'D' = i + (n - i) * 2

main :: IO ()
main = do
    s <- getLine :: IO String
    let n = length s - 1
        a = listArray (0, n) s :: Array Int Char
    print . sum $ [ f a n i | i <- [0..n]]
