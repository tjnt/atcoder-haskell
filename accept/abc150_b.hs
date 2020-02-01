import Data.List

countPartOf :: Eq a => [a] -> [a] -> Int
countPartOf _ [] = 0
countPartOf [] _ = 0
countPartOf a b = if a `isPrefixOf` b
                  then succ $ countPartOf a (drop (length a) b)
                  else countPartOf a (drop 1 b)

main :: IO ()
main = do
    _ <- getLine
    s <- getLine
    print $ countPartOf "ABC" s
