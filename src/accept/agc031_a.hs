import Data.List

main :: IO ()
main = getLine >> getLine >>=
       print . (`mod` (10^9+7)) . pred . product
       . map ((+1) . fromIntegral . length) . group . sort
