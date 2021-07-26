import Data.Array.IArray

trib :: Int -> Int
trib n = m!n
  where
    m = listArray (1,n) (map go [1..n]) :: Array Int Int
    go 1 = 0
    go 2 = 0
    go 3 = 1
    go k = (m!(k-1) + m!(k-2) + m!(k-3)) `rem` 10007

main :: IO ()
main = do
    n <- readLn :: IO Int
    print $ trib n
