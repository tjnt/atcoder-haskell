import Control.Monad
main :: IO ()
main = do
    n <- readLn :: IO Int
    xu <- replicateM n $ (\[x,u] -> (read x :: Double, u)) . words <$> getLine
    print . sum $ map (\(x,u) -> if u == "JPY" then x else x * 380000.0) xu
