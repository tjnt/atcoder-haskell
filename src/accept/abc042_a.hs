import           Data.List
main = getLine >>= putStrLn . f . unwords . sort . words
  where f x = if "5 5 7" == x then "YES" else "NO"
