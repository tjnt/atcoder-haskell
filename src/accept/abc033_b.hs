import Data.List

main :: IO ()
main = do
    _ <- getLine
    xss <- map ((\[a,b] -> (a,read b)) . words)
        . lines <$> getContents :: IO [(String,Int)]
    let g = sum $ map snd xss
        m = find (\(s,p) -> p * 2 > g) xss
    putStrLn $ case m of
        Just (s,p) -> s
        Nothing -> "atcoder"
