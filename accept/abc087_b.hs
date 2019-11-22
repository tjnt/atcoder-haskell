main :: IO ()
main = do
    [a, b, c, x] <- map read . lines <$> getContents
    print . length . filter (==x) . map sum . sequence
        $ [map (500*) [0..a], map (100*) [0..b], map (50*) [0..c]]
