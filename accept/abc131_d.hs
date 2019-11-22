import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)
import Data.List
import Control.Monad

f :: Int -> (Int,Int) -> Maybe Int
f aa (a,b) =
    let aa' = aa + a
      in if aa' <= b then Just aa' else Nothing

main :: IO ()
main = do
    _ <- getLine
    ab <- map ((\[a,b] -> (a,b)) . map (fst . fromJust . BS.readInt) . BS.words)
        . BS.lines <$> BS.getContents :: IO [(Int,Int)]
    putStrLn $
        case foldM f 0 $ sortOn snd ab of
            Just _  -> "Yes"; Nothing -> "No"
