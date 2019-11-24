import Data.Char

conv :: Int -> Char -> Char
conv n c = let i = ord c - 65
            in alpha!!(n+i)
  where
    alpha = ['A'..'Z'] ++ ['A'..'Z']

main :: IO ()
main = do
    n <- readLn :: IO Int
    s <- getLine :: IO String
    putStrLn $ map (conv n) s
