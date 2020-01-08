import Data.Maybe

deleteIndexOf :: Int -> [a] -> [a]
deleteIndexOf i = reverse . foldl (\a (j,x) -> if i == j then a else x:a) [] . zip [0..]

solve :: [Int] -> [Int]
solve xs = reverse $ fromMaybe [-1] (next xs)
  where
    next [] = Just []
    next xs = case filter (uncurry (==)) $ zip xs [1..] of
                [] -> Nothing
                ks -> let k = snd . last $ ks
                       in (:) <$> Just k <*> next (deleteIndexOf (k-1) xs)

main :: IO ()
main = do
    _ <- getLine
    b <- map read . words <$> getLine :: IO [Int]
    mapM_ print $ solve b
