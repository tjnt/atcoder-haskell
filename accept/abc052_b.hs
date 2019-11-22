import Data.List
main :: IO ()
main = do
    _ <- getLine
    s <- getLine :: IO String
    print . fst $ foldl' f (0,0) s
  where
    f (m,p) c = let p' = if c == 'I' then p+1 else p-1
                in (max m p', p')
