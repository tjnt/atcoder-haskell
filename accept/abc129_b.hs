main = do
    n <- read <$> getLine
    w <- map read . words <$> getLine
    let f n a = map sum [take n a, drop n a]
        g a = map (\i -> f i a) [1..(n-1)]
    print . minimum . map (\[x,y] -> abs (x-y)) $ g w
