splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn t xs =
    case break (==t) xs of
        (a,[])  -> [a]
        (a,[_]) -> a:[[]]
        (a,_:b) -> a : splitOn t b

solve :: String -> Int
solve = length . filter ('0' `notElem`) . splitOn '+'

main :: IO ()
main = getLine >>= print . solve
