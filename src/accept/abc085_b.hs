import           Data.List
main = getContents >>= print . length . nub . sort . tail . lines
