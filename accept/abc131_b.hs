import           Data.List
main = do
    [n, l] <- map read . words <$> getLine
    let i = [ x+l | x <- [0..n-1]]
    print . sum . tail . sortBy (\r l -> compare (abs r) (abs l)) $ i
