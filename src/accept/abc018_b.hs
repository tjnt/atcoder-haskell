part :: Int -> Int -> [a] -> [a]
part i n = take n . drop i

solve :: String -> [(Int,Int)] -> String
solve s [] = s
solve s ((l,r):lr) =
    let m = reverse . part l (r-l+1) $ s
        s' = take l s ++ m ++ drop (r+1) s
     in solve s' lr

main :: IO ()
main = do
    s <- getLine
    _ <- getLine
    lr <- map ((\[a,b] -> (a-1,b-1)) . (map read . words))
        . lines <$> getContents :: IO [(Int,Int)]
    putStrLn $ solve s lr
