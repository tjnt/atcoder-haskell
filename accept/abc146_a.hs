import Data.List
import Data.Maybe
main :: IO ()
main = do
    s <- getLine :: IO String
    let s' = reverse ["SUN","MON","TUE","WED","THU","FRI","SAT"]
        i = fromJust $ findIndex (==s) s'
    print (i+1)
