import Control.Monad

main :: IO ()
main = do
    _ <- getLine
    hw <- lines <$> getContents
    forM_ hw $ \s -> replicateM_ 2 $ putStrLn s
