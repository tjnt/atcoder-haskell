primes :: [Int]
primes = 2 : f [3] [3,5..]
  where
    f [] _ = []
    f (x:xs) ys =
        let (ps, qs) = span (< x^2) ys
         in ps ++ f (xs ++ ps) [z | z <- qs, z `rem` x /= 0]

main :: IO ()
main = do
    x <- readLn :: IO Int
    print . head . dropWhile (x>) $ primes
