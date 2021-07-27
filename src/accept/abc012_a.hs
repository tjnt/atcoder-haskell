main :: IO ()
main = interact $ unwords . reverse . words . head . lines
