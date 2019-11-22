import Data.List

uniq :: Eq a => [a] -> Bool
uniq a = length a == length (nub a)

siri :: [String] -> Bool
siri s = all (==True) (siri' s)
  where
    siri' :: [String] -> [Bool]
    siri' [] = []
    siri' [_] = []
    siri' (si:sj:sx) = (last si == head sj) : siri' (sj:sx)

main :: IO ()
main = do
    _ <- getLine
    w <- lines <$> getContents :: IO [String]
    putStrLn $ if uniq w && siri w then "Yes" else "No"
