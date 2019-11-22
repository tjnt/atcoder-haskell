import           Data.List
main = interact $ show .length . nub . words

