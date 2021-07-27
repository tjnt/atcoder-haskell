import Data.Char
import Data.List
import Data.Maybe

main :: IO ()
main = do
    s <- map digitToInt
         . filter (not . isSpace) <$> getLine :: IO [Int]
    k <- readLn :: IO Int
    print $ if all (==1) $ take k s
               then 1 else fromJust $ find (/=1) s
