import           Data.List
import           Data.Maybe

main = do
   s <- getLine
   putStrLn $ if g (Just s) then "YES" else "NO"

f s = fmap (($ s) . stripPrefix) ["dream", "dreamer", "erase", "eraser"]

g :: Maybe String -> Bool
g (Just s) = case f s of
        a | Just "" `elem` a  -> True
          | any isJust a -> any g (filter isJust a)
          | otherwise    -> False
