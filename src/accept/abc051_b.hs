import qualified Data.Set as Set
main = do
    [k,s] <- map read . words <$> getLine
    let a = [ x + y | x <- [0..k], y <- [0..k], x + y <= s ]
        k'= Set.fromList [0..k]
    print $ length [ 1 | xy <- a, Set.member (s - xy) k']
