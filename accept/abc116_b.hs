import qualified Data.Set as Set

f n
  | even n    = n : f (n `div` 2)
  | otherwise = n : f (3 * n + 1)

solve set ((i,x):xs) =
    if Set.member x set
       then i else solve (Set.insert x set) xs

main :: IO ()
main = do
    s <- readLn :: IO Int
    print $ solve Set.empty (zip [1..] (f s))
