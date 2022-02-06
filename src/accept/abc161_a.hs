main :: IO ()
main = do
    [x,y,z] <- words <$> getLine :: IO [String]
    putStrLn $ unwords [z,x,y]
