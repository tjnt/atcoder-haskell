import qualified Data.Map as M
import Data.Maybe (fromMaybe)

solve :: String -> [Int]
solve s = map f "ABCDEF"
  where
    m = M.fromListWith (+) $ zip s (repeat 1)
    f c = fromMaybe 0 $ c `M.lookup` m

main :: IO ()
main = do
    s <- getLine :: IO String
    putStrLn . unwords . map show $ solve s
