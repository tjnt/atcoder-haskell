import Data.List
import Data.Ord

main :: IO ()
main = getLine >> getContents >>=
       putStrLn . head . concat . sortOn (Down . length) . group . sort . lines
