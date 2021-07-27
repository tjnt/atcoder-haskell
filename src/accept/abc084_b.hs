import Data.Char
main :: IO ()
main = do
    [a,b] <- map read . words <$> getLine :: IO [Int]
    s <- getLine :: IO String
    let (t1,t2) = break (=='-') s
        (s1,s2) = (t1, if null t2 then "" else drop 1 t2)
    putStrLn $ if length s1 == a && length s2 == b
                  && all isDigit (s1 ++ s2)
               then "Yes" else "No"
