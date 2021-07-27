main :: IO ()
main = do
    n <- readLn :: IO Int
    print . head . dropWhile (not . same) $ [n..]
  where
    same n = let s = show n
           in all (== head s) s
