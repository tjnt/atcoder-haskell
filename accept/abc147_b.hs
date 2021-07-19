main :: IO ()
main = do
    s <- getLine
    let s' = reverse s
    print $ (`div`2) . length . filter (==False) $ zipWith (==) s s'
