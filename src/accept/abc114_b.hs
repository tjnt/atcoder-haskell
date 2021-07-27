part :: Int -> [a] -> [[a]]
part n a = [(take n . drop i) a | i <- [0..length a - n]]

main :: IO ()
main = do
    s <- getLine
    let nums = map (abs . (-) 753 . read) $ part 3 s :: [Int]
    print $ minimum nums
