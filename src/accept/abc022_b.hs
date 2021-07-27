import Data.List
main :: IO ()
main = getLine >> getContents >>= print . sum . map (pred . length)
     . group . sort . map (read :: String -> Int) . lines
