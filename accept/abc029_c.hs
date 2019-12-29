import Control.Monad
main = readLn >>= \n -> mapM_ putStrLn $ replicateM n "abc"
