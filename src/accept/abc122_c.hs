import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Array.IArray

main :: IO ()
main = do
    _ <- getLine
    s <- BS.unpack <$> BS.getLine
    lr <- map ((\[a,b] -> (a,b)) . map (fst . fromJust . BS.readInt) . BS.words) . BS.lines <$> BS.getContents
    mapM_ print $ solve s lr

solve :: String -> [(Int,Int)] -> [Int]
solve s = arr `seq` map (\(l,r) -> arr!(r-1) - arr!(l-1))
  where
    n = length s
    arr = listArray (0,n+1) (0:f 1 s) :: Array Int Int
    f :: Int -> String -> [Int]
    f _ []  = []
    f _ [_] = []
    f i (s1:s2:sx) =
        let x = if [s1,s2] == "AC" then 1 else 0
        in arr!(i-1) + x : f (i+1) (s2:sx)
