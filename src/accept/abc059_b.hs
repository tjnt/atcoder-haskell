main :: IO ()
main = do
    [a,b] <- lines <$> getContents :: IO [String]
    let res | length a > length b = "GREATER"
            | length a < length b = "LESS"
            | a > b = "GREATER"
            | a < b = "LESS"
            | otherwise = "EQUAL"
     in putStrLn res
