import Data.Array.Unboxed

main :: IO ()
main = do
    c <- listArray ((1,1), (3,3)) . concatMap (map read . words) . lines
        <$> getContents :: IO (Array (Int,Int) Int)
    let b = listArray (1,3) [c!(1,j) | j <- [1..3]] :: Array Int Int
        a = listArray (1,3) [c!(i,1) - (b!1) | i <- [1..3]] :: Array Int Int
    putStrLn $ if all (==True) [ a!i + b!j == c!(i,j) | i <- [1..3], j <- [1..3] ]
                   then "Yes" else "No"
