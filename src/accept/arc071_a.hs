import Data.List

solve :: [String] -> String
solve ss = concatMap (uncurry replicate) $ zip nums chars
  where
    chars = ['a'..'z']
    nums = foldl1 (zipWith min) $ map f ss
    f s = map (\c -> length $ filter (==c) s) chars

main :: IO ()
main = do
    _ <- getLine
    s <- lines <$> getContents
    putStrLn $ solve s
