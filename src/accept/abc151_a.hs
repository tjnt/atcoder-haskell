import Data.List
import Data.Maybe

main :: IO ()
main = do
    c <- getChar
    let a = ['a'..'z']
        i = findIndex (==c) a
     in putChar $ a!!((fromJust i)+1)
