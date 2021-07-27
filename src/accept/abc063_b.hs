import Data.List
main = interact (\s ->
    if length s == (length . nub) s
       then "yes" else "no")
