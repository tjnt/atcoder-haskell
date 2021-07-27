zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (a:as) (b:bs) = x `seq` x : zipWith' f as bs
  where
    x = f a b
zipWith' _ _ _ = []

lucas :: [Int]
lucas = 2 : 1 : zipWith' (+) lucas (tail lucas)

main :: IO ()
main = readLn >>= print . (lucas !!)
