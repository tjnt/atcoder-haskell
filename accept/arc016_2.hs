import Data.List

solve :: [String] -> Int
solve ss = o + x
  where
    x = length . filter ('x'==) $ concat ss
    o = sum . map (length . filter (('o'==) . head) . group) $ transpose ss

main :: IO ()
main = getLine >> getContents >>= print . solve . lines
