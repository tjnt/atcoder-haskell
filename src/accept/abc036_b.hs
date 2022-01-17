import Data.List

main :: IO ()
main = do
    _ <- getLine
    ss <- lines <$> getContents :: IO [String]
    mapM_ (putStrLn . reverse) . transpose $ ss
