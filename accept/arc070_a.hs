import Data.Maybe
import Data.List

main :: IO ()
main = do
    x <- abs <$> readLn :: IO Int
    print $ succ . fromJust . findIndex (x <=) . scanl1 (+) $ [1..]
