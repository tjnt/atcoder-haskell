import Data.List

main :: IO ()
main = do
    _ <- getLine
    hw <- lines <$> getContents :: IO [String]
    let hw' = transpose . transpose . transpose
            . removeLn  . transpose . removeLn
            $ hw
    mapM_ putStrLn hw'
  where
    alldot   = all (=='.')
    removeLn = filter (not . alldot)
