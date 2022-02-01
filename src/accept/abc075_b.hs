import Data.Array.IArray
import Control.Monad
import Data.Char

solve :: Int -> Int -> String -> Array (Int,Int) Char
solve h w xs = listArray ((0,0),(h-1,w-1)) . map f
             $ range ((0,0),(h-1,w-1))
  where
    a = listArray ((0,0),(h-1,w-1)) xs
        :: Array (Int,Int) Char
    b = listArray ((0,0),(h-1,w-1)) $ replicate (h*w) ' '
        :: Array (Int,Int) Char
    c8 (i,j) =
        filter (\(i,j) -> 0 <= i && i <= h-1 && 0 <= j && j <= w-1)
        [ (i+1,j), (i+1,j+1), (i,j+1), (i-1,j+1)
        , (i-1,j), (i-1,j-1), (i,j-1), (i+1,j-1)
        ]
    f c | (a!c) == '#' = '#'
        | otherwise = head . show . sum
                    . map (\c -> if (a!c) == '#' then 1 else 0)
                    $ c8 c

main :: IO ()
main = do
    [h,w] <- map read . words <$> getLine :: IO [Int]
    s <- concat . lines <$> getContents :: IO String
    let r = solve h w s
    forM_ [0..h-1] $ \i -> do
        forM_ [0..w-1] $ \j -> do
            putChar $ r!(i,j)
        putStrLn ""
