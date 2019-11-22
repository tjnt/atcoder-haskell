import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import qualified Data.IntMap as M
import qualified Data.List as L
import Control.Monad
import Text.Printf (printf)

padding :: Int -> String
padding = printf "%06d"

main :: IO ()
main = do
    _ <- getLine
    py <- map ((\[a,b] -> (a,[b])) . map (fst . fromJust . BS.readInt) . BS.words) . BS.lines <$> BS.getContents
    let m = M.map (\v -> M.fromList (zip (L.sort v) [1..]))
          $ M.fromListWith (\[a] b -> a:b) py
    forM_ py (\(p,[y]) -> do
        let v = fromJust $ M.lookup p m
            i = fromJust $ M.lookup y v
        putStr $ padding p
        putStrLn $ padding i)
